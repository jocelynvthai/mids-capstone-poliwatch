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


def transactions_selection(transactions, committee_assignments, subcommittee_assignments):
    col1, col2, col3 = st.columns(3)
    with col1:
        # 1. Select a Politician:
        politician_options = transactions['display_name'].dropna().unique().tolist()
        politician_selection = st.multiselect('1. Select Politician:', ['All'] + politician_options, default=['All'])
        if 'All' in politician_selection:
            filtered_df = transactions.copy()
        else:
            filtered_df = transactions[transactions['display_name'].isin(politician_selection)]
    with col2:
        # 2. Select ticker
        ticker_options = filtered_df['ticker'].dropna().unique().tolist()
        ticker_selection = st.multiselect('2. Select Transaction Ticker:', ['All'] + ticker_options, default=['All'])
        if 'All' not in ticker_selection:
            filtered_df = filtered_df[filtered_df['ticker'].isin(ticker_selection)]
    with col3:
        # 3. Select transaction date
        transaction_date_options = filtered_df['combined_transaction_date'].dropna().unique().tolist()
        transaction_date_selection = st.multiselect('3. Select Transaction Dates:', ['All'] + transaction_date_options, default=['All'])
        if 'All' not in transaction_date_selection:
            filtered_df = filtered_df[filtered_df['combined_transaction_date'].isin(transaction_date_selection)]

    # Display filtered transactions:
    with st.expander(f'Filtered Transactions', expanded=True):
        st.dataframe(filtered_df)

    # Display Transaction Details for the selected politician:
    st.write('Selected Transaction Details:')
    st.dataframe(filtered_df[['amount', 'asset_description', 'type', 'industry', 'sector']])

    # Extract additional information based on the filtered dataframe:
    if politician_selection != 'All':
        politician_id = filtered_df['id'].values[0]
        politician_congress = filtered_df['congress'].values[0]
        politician_committees = committee_assignments[(committee_assignments['member_id'] == politician_id) & (committee_assignments['congress'] == politician_congress)]['committee_id'].values
        politician_subcommittees = subcommittee_assignments[(subcommittee_assignments['member_id'] == politician_id) & (subcommittee_assignments['congress'] == politician_congress)]['subcommittee_id'].values




def main():
    # Create App
    st.title('MIDS Capstone Project: PoliWatch')

    # load data
    data = load_data()

    transactions_selection(data['transactions'], data['committee_assignments'], data['subcommittee_assignments'])

main()








