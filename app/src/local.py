import pandas as pd
import streamlit as st
import altair as alt
import numpy as np
import re
import ast
from sklearn.metrics.pairwise import cosine_similarity
from pygwalker.api.streamlit import StreamlitRenderer, init_streamlit_comm
from PIL import Image, ImageFile
import streamlit_book as stb


def clean_array_string(array_string):
    # Replace newlines and multiple spaces with a single space
    cleaned_string = re.sub(r'\s+', ' ', array_string)
    # Add a comma after each number except the last one
    cleaned_string_with_commas = re.sub(r'([-\d.e]+)(?=\s)', r'\1,', cleaned_string)
    return cleaned_string_with_commas


@st.cache_data
def load_data():
    transactions = pd.read_csv('data/transactions_final.csv')
    committee_assignments = pd.read_csv('data/committee_assignments_final.csv')
    subcommittee_assignments = pd.read_csv('data/subcommittee_assignments_final.csv')
    statements = pd.read_csv('data/member_statements_final.csv')
    travel = pd.read_csv('data/travel_final.csv')
    related_bills = pd.read_csv('data/related_bills_final.csv')
    bills = pd.read_csv('data/bills_final.csv')
    hearings = pd.read_csv('data/committee_hearings_final.csv')

    return {'transactions': transactions,
            'committee_assignments': committee_assignments,
            'subcommittee_assignments': subcommittee_assignments,
            'statements': statements,
            'travel': travel,
            'related_bills': related_bills,
            'bills': bills,
            'hearings': hearings
            }

st.set_page_config(
    layout="wide",  # Use "wide" layout
    initial_sidebar_state="auto",  # Automatically determine the initial state of the sidebar
)

if 'submitted' not in st.session_state:
    st.session_state['submitted'] = False
if 'politician' not in st.session_state:
    st.session_state['politician'] = ''
if 'ticker' not in st.session_state:
    st.session_state['ticker'] = ''
if 'date' not in st.session_state:
    st.session_state['date'] = ''


def politician_graph(placeholder1_title, placeholder1_body, placeholder2_title, placeholder2_body, transactions, politician):
    portfolio_value = transactions.groupby('ticker').last().reset_index()

    bar_chart = alt.Chart(portfolio_value).mark_bar().encode(
        x=alt.X('ticker:N', title='Ticker'),
        y=alt.Y('cumulative_amount', title='Amount ($)'),
        color=alt.condition(
            alt.datum.cumulative_amount > 0,
            alt.value("#AED6E8"),
            alt.value("#F9ACB1")),
        tooltip=[
            alt.Tooltip('ticker', title='Ticker'),
            alt.Tooltip('asset_description', title='Asset Description'),
            alt.Tooltip('cumulative_amount', title='Amount ($)'),
            alt.Tooltip('industry', title='Industry'),
            alt.Tooltip('sector', title='Sector')]
    ).interactive()
    

    industry_bar_chart_data = portfolio_value.fillna("No Industry").groupby('industry').sum('cumulative_amount').reset_index()
    industry_bar_chart = alt.Chart(industry_bar_chart_data).mark_bar().encode(
        x=alt.X('industry', title='Industry'),
        y=alt.Y('cumulative_amount', title='Amount ($)'),
        color=alt.condition(
            alt.datum.cumulative_amount > 0,
            alt.value("#AED6E8"),
            alt.value("#F9ACB1")),
        tooltip=[
            alt.Tooltip('industry', title='Industry'),
            alt.Tooltip('cumulative_amount', title='Amount ($)')]
    ).interactive()

    placeholder1_title.markdown(f"<h1 style='font-size: 20px;'>{politician}'s Estimated Net Holdings At Present by Ticker</h1>", unsafe_allow_html=True)
    placeholder1_body.altair_chart(bar_chart, use_container_width=True)
    placeholder2_title.markdown(f"<h1 style='font-size: 20px;'>{politician}'s Estimated Net Holdings At Present by Industry</h1>", unsafe_allow_html=True)
    placeholder2_body.altair_chart(industry_bar_chart, use_container_width=True)


def politician_ticker_graph(placeholder3_title, placeholder3_body, transactions, politician, ticker):
    transactions = transactions[transactions['ticker'] == ticker].groupby('combined_transaction_date').last().reset_index()

    line_chart = alt.Chart(transactions).mark_line(color='#AED6E8').encode(
        x=alt.X('combined_transaction_date:T', title='Date'),
        y=alt.Y('cumulative_amount:Q', title='Position ($)')
    ).interactive()

    circle_chart = alt.Chart(transactions).mark_circle(size=75, color='#F9ACB1').encode(
        x=alt.X('combined_transaction_date:T', title='Date'),
        y=alt.Y('cumulative_amount:Q', title='Position ($)'),
        tooltip=[
            alt.Tooltip('combined_transaction_date:T', title='Date'),
            alt.Tooltip('cumulative_amount:Q', title='Position ($)')]
    )

    line = alt.Chart(pd.DataFrame({'y': [0]})).mark_rule(strokeDash=[2, 2], color='white').encode(y='y')

    placeholder3_title.markdown(f"<h1 style='font-size: 20px;'>{politician}'s {ticker} Position Over Time</h1>", unsafe_allow_html=True)
    placeholder3_body.altair_chart(line_chart + circle_chart + line, use_container_width=True)


def cumulative(transactions, politician_selection):
    filtered = transactions[(transactions['display_name'] == politician_selection) & (transactions['type'] != 'exchange')].sort_values(by=['combined_transaction_date'])
    filtered['actual_amount'] = filtered.apply(lambda row: row['amount_formatted'] if row['type']=='purchase' else -row['amount_formatted'], axis=1)
    filtered['cumulative_amount'] = filtered.groupby('ticker')['actual_amount'].cumsum()
    return filtered


def relevant_info(data, politician_selection, politician_id, politician_congress, transaction_uuid):

    committee_assignments = data['committee_assignments']
    subcommittee_assignments = data['subcommittee_assignments']
    bills = data['bills']
    committee_hearings = data['hearings']
    travel = data['travel']
    related_bills = data['related_bills']
    member_statements = data['statements']
    transaction = data['transactions']

    st.subheader('')

    transaction_embedding = transaction[transaction['uuid'] == transaction_uuid]['embedding'].values[0]
    transaction_date = transaction[transaction['uuid'] == transaction_uuid]['combined_transaction_date'].values[0]
    transaction_embedding = ast.literal_eval(clean_array_string(transaction_embedding))
    transaction_emb_matrix = np.array(transaction_embedding).reshape(1, -1)

    assignments_hearings_tab, bills_tab, travel_statements_tab = st.tabs(["Assignments & Hearings", "Bills", "Travel & Statements"])
    similarity = 'Cosine similarity correlation score between chosen **transaction** and **congressional activity**'
    with assignments_hearings_tab:
        col1,col2 = st.columns([0.35, 0.65])
        with col1:
            # Politician Committee Assignments
            st.write(f'**{politician_selection}\'s Committee Assignments:**')
            politician_committees = committee_assignments[(committee_assignments['member_id'] == politician_id) & (committee_assignments['congress'] == politician_congress)]
            if len(politician_committees) == 0:
                st.write(f'<div style="margin-left: 20px; color: #3366ff;"><em> no relevant committee assignments</em></div> <br>', unsafe_allow_html=True)
            else:
                politician_committees['clean_embeddings'] = politician_committees['embedding'].apply(lambda x: ast.literal_eval(clean_array_string(x)))
                committee_hearings_emb_matrix = np.vstack(politician_committees['clean_embeddings'].values)
                politician_committees['similarity'] = cosine_similarity(committee_hearings_emb_matrix,transaction_emb_matrix)
                politician_committees_df = politician_committees[['committee_name', 'similarity']].sort_values(by=['similarity'], ascending=False)
                politician_committees_df.columns = ['Committee Name', 'Similarity']
                st.data_editor(politician_committees_df, use_container_width=True, hide_index=True, column_config={"Similarity": st.column_config.NumberColumn(help=similarity)})
        
            # Politician Subcommittee Assignments
            st.write(f'**{politician_selection}\'s Subcommittee Assignments:**')
            politician_subcommittees = subcommittee_assignments[(subcommittee_assignments['member_id'] == politician_id) & (subcommittee_assignments['congress'] == politician_congress)]
            if len(politician_subcommittees) == 0:
                st.write(f'<div style="margin-left: 20px; color: #3366ff;"><em> no relevant subcommittee assignments</em></div> <br>', unsafe_allow_html=True)
            else:
                politician_subcommittees['clean_embeddings'] = politician_subcommittees['embedding'].apply(lambda x: ast.literal_eval(clean_array_string(x)))
                subcommittee_hearings_emb_matrix = np.vstack(politician_subcommittees['clean_embeddings'].values)
                politician_subcommittees['similarity'] = cosine_similarity(subcommittee_hearings_emb_matrix,transaction_emb_matrix)
                politician_subcommittees_df = politician_subcommittees[['subcommittee_name', 'similarity']].sort_values(by=['similarity'], ascending=False)
                politician_subcommittees_df.columns = ['Subcommittee Name', 'Similarity']
                st.data_editor(politician_subcommittees_df, use_container_width=True, hide_index=True, column_config={"Similarity": st.column_config.NumberColumn(help=similarity)})
        with col2:
            # Politician Hearings
            st.write(f'**{politician_selection}\'s Hearings:**')
            politician_committee_assignments = politician_committees['committee_id'].values
            filtered_hearings = committee_hearings[(committee_hearings['hearing_committee_id'].isin(politician_committee_assignments))& (committee_hearings['hearing_congress'] == politician_congress) &(committee_hearings['hearing_date'] <= transaction_date)]
            if len(filtered_hearings) == 0:
                st.write(f'<div style="margin-left: 20px; color: #3366ff;"><em> no relevant hearings</em></div> <br>', unsafe_allow_html=True)
            else:
                hearing_emb_matrix = np.vstack(filtered_hearings['embedding'].apply(lambda x: ast.literal_eval(clean_array_string(x))).values)
                transaction_emb_matrix = np.array(transaction_embedding).reshape(1,-1)
                filtered_hearings['similarity'] = cosine_similarity(hearing_emb_matrix,transaction_emb_matrix)
                filtered_hearings_df = filtered_hearings[['hearing_date', 'hearing_description', 'similarity']].sort_values(by=['similarity'], ascending=False)
                filtered_hearings_df.columns = ['Hearing Date', 'Hearing Description', 'Similarity']
                st.data_editor(filtered_hearings_df, use_container_width=True, hide_index=True, column_config={"Similarity": st.column_config.NumberColumn(help=similarity)}) 
                

    with bills_tab:
        # Politician Bills
        st.write(f'**{politician_selection}\'s Bills:**')
        filtered_bills = bills[(bills['member_id'] == politician_id) & (bills['bill_intro_date'] <= transaction_date)]
        if len(filtered_bills) == 0:
            st.write(f'<div style="margin-left: 20px; color: #3366ff;"><em> no relevant bills</em></div> <br>', unsafe_allow_html=True)
        else:
            bill_emb_matrix = np.vstack(filtered_bills['embedding'].apply(lambda x: ast.literal_eval(clean_array_string(x))).values)
            transaction_emb_matrix = np.array(transaction_embedding).reshape(1,-1)
            filtered_bills['similarity'] = cosine_similarity(bill_emb_matrix,transaction_emb_matrix)
            filtered_bills_df = filtered_bills[['bill_id', 'bill_title', 'bill_intro_date', 'similarity']].sort_values(by=['similarity'], ascending=False)
            filtered_bills_df.columns = ['Bill ID', 'Bill Title', 'Bill Intro Date', 'Similarity']
            st.data_editor(filtered_bills_df, use_container_width=True, hide_index=True, column_config={"Similarity": st.column_config.NumberColumn(help=similarity)})

        # Politician Related Bills
        st.write(f'**{politician_selection}\'s Related Bills:**')
        filtered_related_bills = related_bills[(related_bills['member_id'] == politician_id) & (related_bills['bill_congress'] == politician_congress) &(related_bills['related_bill_introduction_date'] <= transaction_date)]
        if len(filtered_related_bills) == 0:
            st.write(f'<div style="margin-left: 20px; color: #3366ff;"><em> no relevant related bills</em></div> <br>', unsafe_allow_html=True)
        else:
            related_bill_emb_matrix = np.vstack(filtered_related_bills['embedding'].apply(lambda x: ast.literal_eval(clean_array_string(x))).values)
            transaction_emb_matrix = np.array(transaction_embedding).reshape(1,-1)
            filtered_related_bills['similarity'] = cosine_similarity(related_bill_emb_matrix,transaction_emb_matrix)
            filtered_related_bills_df = filtered_related_bills[['related_bill_id', 'related_bill_title', 'related_bill_introduction_date', 'similarity']].sort_values(by=['similarity'], ascending=False)
            filtered_related_bills_df.columns = ['Related Bill ID', 'Related Bill Title', 'Related Bill Intro Date', 'Similarity']
            st.data_editor(filtered_related_bills_df, use_container_width=True, hide_index=True, column_config={"Similarity": st.column_config.NumberColumn(help=similarity)})

    with travel_statements_tab:
        # Politician Travel
        st.write(f'**{politician_selection}\'s Travel:**')
        filtered_travel = travel[(travel['member_id'] == politician_id) & (travel['congress'] == politician_congress) &(travel['departure_date'] <= transaction_date)]
        if len(filtered_travel) == 0:
            st.write(f'<div style="margin-left: 20px; color: #3366ff;"><em> no relevant travel</em></div> <br>', unsafe_allow_html=True)
        else:
            travel_emb_matrix = np.vstack(filtered_travel['embedding'].apply(lambda x: ast.literal_eval(clean_array_string(x))).values)
            transaction_emb_matrix = np.array(transaction_embedding).reshape(1,-1)
            filtered_travel['similarity'] = cosine_similarity(travel_emb_matrix,transaction_emb_matrix)
            filtered_travel_df = filtered_travel[['departure_date', 'destination', 'sponsor', 'similarity']].sort_values(by=['similarity'], ascending=False)
            filtered_travel_df.columns = ['Departure Date', 'Destination', 'Sponsor', 'Similarity']
            st.data_editor(filtered_travel_df, use_container_width=True, hide_index=True, column_config={"Similarity": st.column_config.NumberColumn(help=similarity)})

        # Politician Statements
        st.write(f'**{politician_selection}\'s Statements:**')
        filtered_statements = member_statements[(member_statements['member_id'] == politician_id) & (member_statements['congress'] == politician_congress) &(member_statements['date'] <= transaction_date)]
        if len(filtered_statements) == 0:
            st.write(f'<div style="margin-left: 20px; color: #3366ff;"><em> no relevant statements</em></div> <br>', unsafe_allow_html=True)
        else:
            statement_emb_matrix = np.vstack(filtered_statements['embedding'].apply(lambda x: ast.literal_eval(clean_array_string(x))).values)
            transaction_emb_matrix = np.array(transaction_embedding).reshape(1,-1)
            filtered_statements['similarity'] = cosine_similarity(statement_emb_matrix,transaction_emb_matrix)
            filtered_statements_df = filtered_statements[['title', 'date', 'type', 'similarity','url']].sort_values(by=['similarity'], ascending=False)
            filtered_statements_df.columns = ['Title', 'Date', 'Type', 'Similarity','URL']
            st.data_editor(filtered_statements_df, use_container_width=True, hide_index=True, 
                           column_config={"Similarity": st.column_config.NumberColumn(help=similarity), 
                                          "URL": st.column_config.LinkColumn()})


def transactions_selection(data):
    transactions = data['transactions']

    politician, ticker, date, submit = st.columns([0.29, 0.29, 0.29, 0.13])
    with politician:
        politician_options = transactions['display_name'].dropna().unique().tolist()
        politician_selection = st.selectbox('(1) Select Politician:', politician_options)
        filtered_df = transactions[transactions['display_name'] == politician_selection]
        if st.session_state['politician'] != politician_selection:
            st.session_state['politician'] = politician_selection
            st.session_state['submitted'] = False
    with ticker:
        ticker_options = filtered_df['ticker'].dropna().unique().tolist()
        ticker_selection = st.selectbox('(2) Select Transaction Ticker:', ticker_options)
        filtered_df = filtered_df[filtered_df['ticker'] == ticker_selection]
        if st.session_state['ticker'] != ticker_selection:
            st.session_state['ticker'] = ticker_selection
            st.session_state['submitted'] = False
    with date:
        transaction_date_options = filtered_df['combined_transaction_date'].dropna().unique().tolist()
        transaction_date_selection = st.selectbox('(3) Select Transaction Date:', transaction_date_options)
        filtered_df = filtered_df[filtered_df['combined_transaction_date'] == transaction_date_selection]
        if st.session_state['date'] != transaction_date_selection:
            st.session_state['date'] = transaction_date_selection
            st.session_state['submitted'] = False
    with submit:
        st.markdown(f'<style>div.stSelectbox > div[role="button"] > div {{font-size: 0.5px;}}</style>', unsafe_allow_html=True)
        st.markdown(f'<style>div.stSelectbox > div[role="button"] > div {{font-size: 0.5px;}}</style>', unsafe_allow_html=True)
        submitted = st.button("Fetch Transaction(s)")

    placeholder1_title = st.empty()
    placeholder1_body = st.empty()
    placeholder2_title = st.empty()
    placeholder2_body = st.empty()
    placeholder3_title = st.empty()
    placeholder3_body = st.empty()

    if not (st.session_state['submitted'] or submitted):
        cumulative_df = cumulative(transactions, politician_selection)
        politician_graph(placeholder1_title, placeholder1_body, placeholder2_title, placeholder2_body, cumulative_df, politician_selection)
        politician_ticker_graph(placeholder3_title, placeholder3_body, cumulative_df, politician_selection, ticker_selection)

    if submitted or st.session_state['submitted']:
        st.session_state.submitted = True
        st.subheader('Selected Transaction Details:')
        display_df = filtered_df[['congress', 'disclosure_date', 'asset_description', 'industry', 'sector', 'type', 'amount', 'state', 'ptr_link']]
        display_df.columns = ["Congress", "Disclosure Date", "Asset Description", "Industry", "Sector", "Type", "Amount", "State", "PTR Link"]
        st.data_editor(display_df, column_config={"PTR Link": st.column_config.LinkColumn()}, hide_index=True)

        politician_id = filtered_df['member_id'].values[0]
        politician_congress = filtered_df['congress'].values[0]
        transaction_uuid = filtered_df['uuid'].values[0]

        generate_relevant_info = st.button('Generate Relevant Info')
        if generate_relevant_info:
                relevant_info(data, politician_selection, politician_id, politician_congress, transaction_uuid)


def about(data):

    stb.set_chapter_config(path='src/book', button="top", button_previous="←",
                        button_next="→",button_refresh="Refresh", display_page_info=False)

    
    # st.markdown(f"<h1 style='font-size: 20px;'>Entity Relationship Diagram (ERD)</h1>", unsafe_allow_html=True)
    # url = "https://drawsql.app/teams/jocelyn-thai/diagrams/poliwatch/embed"
    # iframe_code = f'<iframe src="{url}" width="100%" height="500" frameborder="0" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true"></iframe>'
    # st.markdown(iframe_code, unsafe_allow_html=True)


def pygwalker(data):
    option = st.selectbox('Select Dataset', ('Transactions', 'Committee Assignments', 'Subcommittee Assignments', 'Statements', 'Travel', 'Related Bills', 'Bills', 'Hearings'))
    dataset_name = {'Transactions': 'transactions', 'Committee Assignments': 'committee_assignments', 
               'Subcommittee Assignments': 'subcommittee_assignments','Statements': 'statements', 'Travel': 'travel',
                'Related Bills': 'related_bills', 'Bills': 'bills', 'Hearings': 'hearings'}
    selected_dataset = dataset_name[option]
    dataset = data[selected_dataset].drop(columns=['Unnamed: 0.1', 'Unnamed: 0'])

    # Establish communication between pygwalker and streamlit
    init_streamlit_comm()
    
    # Get an instance of pygwalker's renderer. You should cache this instance to effectively prevent the growth of in-process memory.
    @st.cache_resource(hash_funcs={StreamlitRenderer: lambda _: None})
    def get_pyg_renderer(dataset) -> "StreamlitRenderer":
        # When you need to publish your app to the public, you should set the debug parameter to False to prevent other users from writing to your chart configuration file.
        return StreamlitRenderer(dataset, spec="./gw_config.json", debug=False)
    
    renderer = get_pyg_renderer(dataset)
    
    # Render your data exploration interface. Developers can use it to build charts by drag and drop.
    renderer.render_explore()
   


def footer():
    st.markdown(
        """
        <div class="footer" style="position: fixed; bottom: 0; width: 90%; text-align: right;">
            <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/a1/Seal_of_University_of_California%2C_Berkeley.svg/2048px-Seal_of_University_of_California%2C_Berkeley.svg.png" width="80" style="margin-bottom: 1px;">
            <br>  
            <p style="margin-bottom: 1px; font-size: 12px;">5th Year MIDS</p>
            <p style="margin-bottom: 1px; font-size: 12px;">Capstone Project (Fall 2023)</p>
            <p style="margin-bottom: 10px; font-size: 10px;"> Matthew Dodd, Aditya Shah, Jocelyn Thai, Connor Ethan Yen</p>
        </div>
        """,
        unsafe_allow_html=True
    )


def main():
    data = load_data()
    st.title('PoliWatch :flag-us:')
    st.subheader('U.S. Congressional Securities Transactions')
    
    about_tab, interactive_data_explore_tab, trading_activity_tab = st.tabs(["About", "Interactive Data Explore", "Trading Activity"])
    with about_tab:
        about(data)
    with interactive_data_explore_tab:
        pygwalker(data)
    with trading_activity_tab:
        transactions_selection(data)


if __name__ == "__main__":
    main()
    footer()
