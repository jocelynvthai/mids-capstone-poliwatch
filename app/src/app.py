import pandas as pd
import streamlit as st
import altair as alt
from utils import set_db_connection

# Set page configuration
st.set_page_config(
    layout="wide",  # Use "wide" layout
    initial_sidebar_state="auto",  # Automatically determine the initial state of the sidebar
)

# Set DB Connection
s3 = set_db_connection()

# Session State
if 'submitted' not in st.session_state:
    st.session_state['submitted'] = False
if 'politician' not in st.session_state:
    st.session_state['politician'] = ''
if 'ticker' not in st.session_state:
    st.session_state['ticker'] = ''
if 'date' not in st.session_state:
    st.session_state['date'] = ''


# Load data with cache
@st.cache_data
def load_data():
    bucket = s3.Bucket('mids-capstone')
    transactions_obj = bucket.Object('transactions/transactions.csv').get()
    committee_assignments_obj = bucket.Object('assignments/committee_assignments_of_interest.csv').get()
    subcommittee_assignments_obj = bucket.Object('assignments/subcommittee_assignments_of_interest.csv').get()
    committees_obj = bucket.Object('committees/committees.csv').get()
    subcommittees_obj = bucket.Object('subcommittees/subcommittees.csv').get()
    bills_obj = bucket.Object('bills/member_bills.csv').get()
    committee_hearings_obj = bucket.Object('hearings/committee_hearings.csv').get()
    travel_obj = bucket.Object('travel/private_travel.csv').get()
    related_bills_obj = bucket.Object('bills/related_bills.csv').get()
    member_statements_obj = bucket.Object('statements/member_statements.csv').get()

    return {
        'transactions': pd.read_csv(transactions_obj['Body']),
        'committee_assignments': pd.read_csv(committee_assignments_obj['Body']),
        'subcommittee_assignments': pd.read_csv(subcommittee_assignments_obj['Body']),
        'committees': pd.read_csv(committees_obj['Body']),
        'subcommittees': pd.read_csv(subcommittees_obj['Body']),
        'bills': pd.read_csv(bills_obj['Body']),
        'committee_hearings': pd.read_csv(committee_hearings_obj['Body']),
        'travel': pd.read_csv(travel_obj['Body']),
        'related_bills': pd.read_csv(related_bills_obj['Body']),
        'member_statements': pd.read_csv(member_statements_obj['Body'])
    }


def transactions_selection(data):
    # data
    transactions = data['transactions']

    politician, ticker, date, submit = st.columns([0.305, 0.305, 0.305, 0.085])
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
        st.markdown(f'<style>div.stSelectbox > div[role="button"] > div {{font-size: 1px;}}</style>', unsafe_allow_html=True)
        st.markdown(f'<style>div.stSelectbox > div[role="button"] > div {{font-size: 1px;}}</style>', unsafe_allow_html=True)
        submitted = st.button("Submit")

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
        st.subheader('Selected Transaction Details:', divider='red')
        display_df = filtered_df[['congress', 'disclosure_date', 'asset_description', 'industry', 'sector', 'type', 'amount', 'state', 'ptr_link']]
        display_df.columns = ["Congress", "Disclosure Date", "Asset Description", "Industry", "Sector", "Type", "Amount", "State", "PTR Link"]
        st.data_editor(display_df,
            column_config={"PTR Link": st.column_config.LinkColumn(help="Periodic Transaction Report")}, 
            hide_index=True)
        
        politician_id = filtered_df['id'].values[0]
        politician_congress = filtered_df['congress'].values[0]

        generate_relevant_info = st.button('Generate Relevant Info')
        if generate_relevant_info:
            relevant_info(data, politician_selection, politician_id, politician_congress)


def cumulative(transactions, politician_selection):
    filtered = transactions[(transactions['display_name'] == politician_selection) & (transactions['type'] != 'exchange')].sort_values(by=['combined_transaction_date'])
    filtered['actual_amount'] = filtered.apply(lambda row: row['amount_formatted'] if row['type']=='purchase' else -row['amount_formatted'], axis=1)
    filtered['cumulative_amount'] = filtered.groupby('ticker')['actual_amount'].cumsum()
    test = filtered[filtered['ticker']=='CNC'][['ticker', 'combined_transaction_date', 'type', 'amount_formatted', 'actual_amount', 'cumulative_amount']]
    return filtered
    

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


def relevant_info(data, politician_selection, politician_id, politician_congress):
    # data
    committee_assignments = data['committee_assignments']
    subcommittee_assignments = data['subcommittee_assignments']
    committees = data['committees']
    subcommittees = data['subcommittees']
    bills = data['bills']
    committee_hearings = data['committee_hearings']
    travel = data['travel']
    related_bills = data['related_bills']
    member_statements = data['member_statements']

    st.subheader('', divider='blue')

    politician_committees = committee_assignments[(committee_assignments['member_id'] == politician_id) & (committee_assignments['congress'] == politician_congress)]['committee_id'].values
    politician_subcommittees = subcommittee_assignments[(subcommittee_assignments['member_id'] == politician_id) & (subcommittee_assignments['congress'] == politician_congress)]['subcommittee_id'].values

    # Display Politician's Committee Assignments:
    st.write(f'**{politician_selection}\'s Committee Assignments:**')
    filtered_committees = committees[(committees['committee_id'].isin(politician_committees)) & (committees['congress'] == politician_congress)][['committee_id', 'committee_name', 'congress', 'chamber']]
    filtered_committees.columns = ['Committee ID', 'Committee Name', 'Congress', 'Chamber']
    if len(filtered_committees) == 0:
        st.write(f'<div style="margin-left: 20px; color: #3366ff;"><em> no relevant committee assignments</em></div> <br>', unsafe_allow_html=True)
    else:
        st.data_editor(filtered_committees, hide_index=True)

    # Display Politician's Subcommittee Assignments:
    st.write(f'**{politician_selection}\'s Subcommittee Assignments:**')
    filtered_subcommittees = subcommittees[(subcommittees['subcommittee_id'].isin(politician_subcommittees)) & (subcommittees['congress'] == politician_congress)][['subcommittee_id', 'subcommittee_name', 'congress', 'chamber']]
    filtered_subcommittees.columns = ['Subcommittee ID', 'Subcommittee Name', 'Congress', 'Chamber']
    if len(filtered_subcommittees) == 0:
        st.write(f'<div style="margin-left: 20px; color: #3366ff;"><em> no relevant subcommittee assignments</em></div> <br>', unsafe_allow_html=True)
    else:
        st.data_editor(filtered_subcommittees, hide_index=True)

    # Display Politician's Bills
    st.write(f'**{politician_selection}\'s Bills:**')
    filtered_bills = bills[bills['member_id'] == politician_id][['bill_id', 'bill_title', 'bill_intro_date', 'bill_summary', 'bill_url']]
    filtered_bills.columns = ['Bill ID', 'Bill Title', 'Bill Intro Date', 'Bill Summary', 'Bill Link']
    if len(filtered_bills) == 0:
        st.write(f'<div style="margin-left: 20px; color: #3366ff;"><em> no relevant bills</em></div> <br>', unsafe_allow_html=True)
    else:
        st.data_editor(filtered_bills,
            column_config={"Bill Link": st.column_config.LinkColumn()}, 
            hide_index=True)

    # Display Politician's Hearings
    st.write(f'**{politician_selection}\'s Hearings:**')
    politician_committees = filtered_committees['Committee ID'].values
    filtered_hearings = committee_hearings[(committee_hearings['hearing_committee_id'].isin(politician_committees)) & (committee_hearings['hearing_congress'] == politician_congress)][['hearing_date', 'hearing_description']]
    filtered_hearings.columns = ['Hearing Date', 'Hearing Description']
    if len(filtered_hearings) == 0:
        st.write(f'<div style="margin-left: 20px; color: #3366ff;"><em> no relevant hearings</em></div> <br>', unsafe_allow_html=True)
    else:
        st.data_editor(filtered_hearings, hide_index=True)

    # Display Politician's Travel
    st.write(f'**{politician_selection}\'s Travel:**')
    filtered_travel = travel[(travel['member_id'] == politician_id) & (travel['congress'] == politician_congress)][['departure_date', 'destination', 'sponsor']]
    filtered_travel.columns = ['Departure Date', 'Destination', 'Sponsor']
    if len(filtered_travel) == 0:
        st.write(f'<div style="margin-left: 20px; color: #3366ff;"><em> no relevant travel</em></div> <br>', unsafe_allow_html=True)
    else:
        st.data_editor(filtered_travel, hide_index=True)

    # Display Politician's Related Bills
    st.write(f'**{politician_selection}\'s Related Bills:**')
    filtered_related_bills = related_bills[(related_bills['member_id'] == politician_id) & (related_bills['bill_congress'] == politician_congress)][['related_bill_id', 'related_bill_title', 'related_bill_introduction_date']]
    filtered_related_bills.columns = ['Related Bill ID', 'Related Bill Title', 'Related Bill Introduction Date']
    if len(filtered_related_bills) == 0:
        st.write(f'<div style="margin-left: 20px; color: #3366ff;"><em> no relevant related bills</em></div> <br>', unsafe_allow_html=True)
    else:
        st.data_editor(filtered_related_bills, hide_index=True)

    # Display Politician's Statements
    st.write(f'**{politician_selection}\'s Statements:**')
    filtered_statements = member_statements[(member_statements['member_id'] == politician_id) & (member_statements['congress'] == politician_congress)][['type', 'date', 'title', 'url']]
    filtered_statements.columns = ['Type', 'Date', 'Title', 'Statement Link']
    if len(filtered_statements) == 0:
        st.write(f'<div style="margin-left: 20px; color: #3366ff;"><em> no relevant statements</em></div> <br>', unsafe_allow_html=True)
    else:
        st.data_editor(filtered_statements,
            column_config={"Statement Link": st.column_config.LinkColumn()}, 
            hide_index=True)



def footer():
    st.markdown(
        """
        <div class="footer" style="position: fixed; bottom: 0; width: 90%; text-align: right;">
            <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/a1/Seal_of_University_of_California%2C_Berkeley.svg/2048px-Seal_of_University_of_California%2C_Berkeley.svg.png" width="80" style="margin-bottom: 1px;">
            <br>  
            <p style="margin-bottom: 1px; font-size: 12px;">Capstone Project (Fall 2023)</p>
            <p style="margin-bottom: 10px; font-size: 10px;"> Matthew Dodd, Aditya Shah, Jocelyn Thai, Connor Ethan Yen</p>
        </div>
        """,
        unsafe_allow_html=True
    )


def main():
    data = load_data()
    st.title('PoliWatch :flag-us:')
    st.subheader('U.S. Congressional Securities Transactions', divider='red')
    transactions_selection(data)

if __name__ == "__main__":
    main()
    footer()






