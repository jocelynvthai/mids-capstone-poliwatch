
import pandas as pd
import streamlit as st
from utils import set_db_connection

# Set page configuration
st.set_page_config(
    page_title="MIDS Capstone Project: PoliWatch",
    layout="wide",  # Use "wide" layout
    initial_sidebar_state="auto",  # Automatically determine the initial state of the sidebar
)

# Set DB Connection
s3 = set_db_connection()

# Load data
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

transactions = pd.read_csv(transactions_obj['Body'])
committee_assignments = pd.read_csv(committee_assignments_obj['Body'])
subcommittee_assignments = pd.read_csv(subcommittee_assignments_obj['Body'])
committees = pd.read_csv(committees_obj['Body'])
subcommittees = pd.read_csv(subcommittees_obj['Body'])
bills = pd.read_csv(bills_obj['Body'])
committee_hearings = pd.read_csv(committee_hearings_obj['Body'])
travel = pd.read_csv(travel_obj['Body'])
related_bills = pd.read_csv(related_bills_obj['Body'])
member_statements = pd.read_csv(member_statements_obj['Body'])


# Create App
st.title('MIDS Capstone Project: PoliWatch')

# Select a Politician to view their transactions:
politician_options = transactions['display_name'].dropna().unique().tolist()
politician_selection = st.selectbox('Select a Politician:', politician_options)
filtered_df = transactions[transactions['display_name'] == politician_selection]
with st.expander(f'Transactions', expanded=True):
    st.dataframe(filtered_df)


# Select a Transaction to view its details:
col1, col2 = st.columns(2)
with col1:
    st.write("Transaction Ticker Selection")
    transactions_options = filtered_df['ticker'].dropna().unique().tolist()
    transaction_selection = st.selectbox('Select a Ticker:', transactions_options)
    filtered_df = filtered_df[filtered_df['ticker'] == transaction_selection]

    with st.expander(f'Here are {politician_selection}\'s transactions for {transaction_selection}:', expanded=True):
        st.dataframe(filtered_df)
with col2:
    st.write("Transaction Date Selection")
    transaction_date_options = filtered_df['combined_transaction_date'].dropna().unique().tolist()
    transaction_date_selection = st.selectbox('Select a Transaction Date:', transaction_date_options)
    transaction_row = filtered_df[filtered_df['combined_transaction_date'] == transaction_date_selection]

# Display Transaction Details:
st.write('Selected Transaction Details:')
st.dataframe(transaction_row[['amount', 'asset_description', 'type', 'industry', 'sector']])

politician_id = filtered_df['id'].values[0]
politician_congress = filtered_df['congress'].values[0]
politician_committees = committee_assignments[(committee_assignments['member_id'] == politician_id) & (committee_assignments['congress'] == politician_congress)]['committee_id'].values
politician_subcommittees = subcommittee_assignments[(subcommittee_assignments['member_id'] == politician_id) & (subcommittee_assignments['congress'] == politician_congress)]['subcommittee_id'].values


st.header('Additional Information: This is what we need the model to filter for.')

# Display Politician's Committee Assignments:
st.write('Selected Politician\'s Committee Assignments:')
filtered_committees =committees[(committees['committee_id'].isin(politician_committees)) & (committees['congress'] == politician_congress)][['committee_id', 'committee_name', 'congress', 'chamber']]
st.dataframe(filtered_committees)

# Display Politician's Subcommittee Assignments:
st.write('Selected Politician\'s Subcommittee Assignments:')
st.dataframe(subcommittees[(subcommittees['subcommittee_id'].isin(politician_subcommittees)) & (subcommittees['congress'] == politician_congress)][['subcommittee_id', 'subcommittee_name', 'congress', 'chamber']])


# Display Politician's Bills

filtered_bills = bills[bills['member_id'] == politician_id]
st.write('Selected Politician\'s Bills:')
st.dataframe(filtered_bills[['bill_id', 'bill_title', 'bill_intro_date', 'bill_summary', 'bill_url']])

# Display Politician's Hearings""
politician_committees = filtered_committees['committee_id'].values

filtered_hearings = committee_hearings[(committee_hearings['hearing_committee_id'].isin(politician_committees))& (committee_hearings['hearing_congress'] == politician_congress)]
st.write('Selected Politician\'s Hearings:')
st.dataframe(filtered_hearings[['hearing_date', 'hearing_description']])


# Display Politician's Travel""
filtered_travel = travel[(travel['member_id'] == politician_id) & (travel['congress'] == politician_congress)]
st.write('Selected Politician\'s Travel:')
st.dataframe(filtered_travel[['departure_date', 'destination', 'sponsor']])

# Display Politician's Related Bills""
filtered_related_bills = related_bills[(related_bills['member_id'] == politician_id) & (related_bills['bill_congress'] == politician_congress)]
st.write('Selected Politician\'s Related Bills:')
st.dataframe(filtered_related_bills[['related_bill_id', 'related_bill_title', 'related_bill_introduction_date']])

# Display Politician's Statements""
filtered_statements = member_statements[(member_statements['member_id'] == politician_id) & (member_statements['congress'] == politician_congress)]
st.write('Selected Politician\'s Statements:')
st.dataframe(filtered_statements[['type', 'date', 'title', 'url']])









