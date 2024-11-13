# Install the required packages if they are not already installed
!pip install python-dotenv beautifulsoup4 requests transformers

# Imports
import os
import requests
from dotenv import load_dotenv
from bs4 import BeautifulSoup
from IPython.display import Markdown, display
from transformers import pipeline

# Load environment variables (if using a .env file, otherwise this step is optional)
load_dotenv()

# A class to represent a Webpage
class Website:
    url: str
    title: str
    text: str

    def __init__(self, url):
        self.url = url
        # Send a GET request to the provided URL
        headers = {
            "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3"
        }
        response = requests.get(url, headers=headers)
        
        # Check if the request was successful
        if response.status_code == 200:
            # Use BeautifulSoup to parse the HTML content
            soup = BeautifulSoup(response.content, 'html.parser')
            # Extract the title of the webpage
            self.title = soup.title.string if soup.title else "No title found"
            # Remove irrelevant elements like script, style, img, and input for cleaner text
            for irrelevant in soup.body(["script", "style", "img", "input"]):
                irrelevant.decompose()
            # Extract clean text from the HTML body
            self.text = soup.body.get_text(separator="\n", strip=True)
        else:
            self.title = "Failed to retrieve page"
            self.text = f"Error {response.status_code}: Could not retrieve content from the URL"


# Function to generate a summary using Pegasus
def summarize(url):
    website = Website(url)
    summarizer = pipeline("summarization", model="google/pegasus-xsum")  # Using Pegasus model for summarization
    #summarizer = pipeline("summarization", model="facebook/bart-large-cnn")  # Using BART model for summarization
    #summarizer = pipeline("summarization", model="t5-small")  # Using T5 model for summarization
    # Preprocess the text: limit the length for the summarization model
    input_text = website.text[:2000]  # Adjust as needed; Both Pegasus & BART can handle longer inputs than T5. For T5 use 1000
    # Handle edge cases when the text is too short or empty
    if not input_text.strip():
        return "No content available to summarize."
    # Generate the summary
    summary_output = summarizer(input_text, max_length=200, min_length=60, do_sample=False)
    summary = summary_output[0]['summary_text']
    return summary

# Display the summary using Markdown for better readability in Colab
def display_summary(url):
    summary = summarize(url)
    display(Markdown(f"### Summary of {url}:\n\n{summary}"))

# Example usage with the specified URL
display_summary("https://en.wikipedia.org/wiki/Main_Page")
