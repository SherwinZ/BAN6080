import sys
import os
try:
    import yfinance as yf
    import pandas as pd
    import pandas_datareader.data as web
except ImportError as e:
    print(f"A required library is not installed: {e}")
    print("Please install the required libraries by running:")
    print("pip install yfinance pandas pandas-datareader")
    sys.exit(1)

def get_sp500_tickers():
    """
    Scrapes the list of S&P 500 tickers from Wikipedia.
    """
    sp500_tickers = pd.read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies')[0]
    # The symbol column may need to be cleaned to remove any characters that are not valid in file names or tickers
    sp500_tickers['Symbol'] = sp500_tickers['Symbol'].str.replace('.', '-', regex=False)
    return sp500_tickers['Symbol'].tolist()

def main():
    """
    Main function to execute the workflow.
    """
    # 1. Get S&P 500 tickers
    tickers = get_sp500_tickers()
    
    # 2. Define date range (last 10 years from today)
    end_date = pd.to_datetime('today')
    start_date = end_date - pd.DateOffset(years=10)

    # 3. Fetch historical data
    # Using yfinance to download the data
    # auto_adjust=True provides adjusted closing prices in the 'Close' column
    try:
        data = yf.download(tickers, start=start_date, end=end_date, progress=True, auto_adjust=True)
        
        # Check if data was downloaded
        if data.empty:
            print("No data downloaded. Please check the tickers and date range.")
            return

        # The 'Close' column now contains the adjusted close prices
        close_prices = data['Close']

        # 4. Calculate monthly returns
        # Resample to monthly frequency, using the last day's price
        monthly_prices = close_prices.resample('M').last()
        monthly_returns = monthly_prices.pct_change()
        
        # 5. Clean and save returns to returns.csv
        # Drop the first row of returns, which will be NaN
        monthly_returns = monthly_returns.iloc[1:]
        # Drop columns that are all NaN (for stocks that may not have data for the full period)
        monthly_returns = monthly_returns.dropna(axis=1, how='all')
        
        output_dir = r"C:\WFU\Courses\Spring\Financial_Risk_Analysis\portfolio"
        if not os.path.exists(output_dir):
            os.makedirs(output_dir)
            
        output_path = os.path.join(output_dir, 'returns.csv')
        monthly_returns.to_csv(output_path)
        
        print("Successfully downloaded data and calculated monthly returns.")
        print(f"Results for {len(monthly_returns.columns)} stocks saved to {output_path}")

    except Exception as e:
        print(f"An error occurred during the data download and processing: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()
