from google import genai
import json
import time
import os
import pymupdf

# API configuration
GEMINI_API_KEY = "YOUR_API_KEY_HERE"
MODEL = "gemini-2.5-flash"
client = genai.Client(api_key=GEMINI_API_KEY)

# PDF path configuration
PDF_PATH = "path/to/document.pdf" # insert the path to the PDF you want to process

def read_pdf(pdf_path):
    if not os.path.exists(pdf_path):
        raise FileNotFoundError(f'Could not find the PDF file at: {pdf_path}')

    print(f'Reading PDF: {pdf_path}')
    doc = pymupdf.open(pdf_path)
    full_text = ''

    for page_number, page in enumerate(doc, 1):
        page_text = page.get_text()
        full_text += page_text
        print(f'Page {page_number}/{len(doc)} read')

    doc.close()

    if not full_text.strip():
        raise ValueError("The PDF does not contain any readable text")

    print(f'Total characters extracted: {len(full_text)}')
    print(f'Total words: {len(full_text.split())}')

    return full_text.strip()

# Questions for requirement 3
QUESTIONS = [
                "What is the main subject of the document?",
                "What are the 3 most important conclusions from the text?"
            ]

# Functions for interacting with the LLM
# Call Google Gemini and return response
def call_llm(prompt):
    response = client.models.generate_content(
        model = MODEL,
        contents = prompt
    )
    return response.text

# Function for formatting text output in the console
def display_section(title, content, width=70):
    print(f"\n{'=' * width}")
    print(f"{title}")
    print(f"{'=' * width}")
    print(content.strip())
    print(f"{'=' * width}")

# Function that returns the result and elapsed time in seconds
def measure_time(func, *args):
    start = time.time()
    result = func(*args)
    duration = round(time.time() - start, 2)
    return result, duration

# 1. Short summary (4-5 lines)
def generate_summary(text):
    prompt = f"""
    You are an assistant specialized in text summarization.
    Summarize the following text in a maximum of 4-5 lines, in English.
    Focus on the main idea and essential conclusions.
    Do not use bullet points. Write a coherent paragraph.
    
    TEXT: {text}
    SUMMARY: 
    """
    return call_llm(prompt)

# 2. Important information from the text
def extract_information(text):
    prompt = f"""
    You are a precise and concise text analyst.
    Extract exactly 5 important pieces of information from the text below.
    Present them as a numbered list (1-5), each on a separate line.
    Each point must contain a specific factual piece of information from the text
    (numbers, percentages, names, concrete facts).
    
    TEXT: {text}
    KEY INFORMATION: 
    """
    return call_llm(prompt)

# 3. Questions based on the text (2)
def answer_question(text, question):
    prompt = f"""
    You are an assistant that answers questions based EXCLUSIVELY on the provided text.
    If the answer is not found in the text, state this explicitly.
    Answer in 2-3 clear, concise sentences in English.
    
    TEXT: {text}
    QUESTION: {question}
    ANSWER:
    """
    return call_llm(prompt)

# 4. Prompt comparison

PROMPT_VARIANT_A = 'Summarize the following text in 4-5 lines: {text}'
PROMPT_VARIANT_B = """You are an expert in document analysis and professional communication.
Your task: create a 4-5 line executive summary for the text below.
Specific instructions:
- Mention the main subject of the document
- Include at least one concrete example or specific data point from the text
- Highlight the central idea or conclusion of the document
- End with a synthesizing sentence
- Tone: professional, neutral, informative
TEXT:
{text}
EXECUTIVE SUMMARY:"""

# Run both prompt variants and return results
def compare_prompts(text):
    result_a, time_a = measure_time(call_llm, PROMPT_VARIANT_A.format(text=text))

    result_b, time_b = measure_time(call_llm, PROMPT_VARIANT_B.format(text=text))

    return {
        "variant_a": {"result": result_a, "time": time_a},
        "variant_b": {"result": result_b, "time": time_b}
    }

# 5. Analysis
ANALYSIS = """
    Approach:
    We used the Google Gemini API (gemini-2.5-flash) together with the PyMuPDF library for extracting text from PDF files.
    Each task (summary, extraction, questions, comparison) has a dedicated function with a dedicated prompt.
    The source text is loaded dynamically from the PDF, making the script reusable for any document.
    
    Prompt comparison:
    Variant A produces a generic summary without a clear structure because the prompt provided is minimal, with no details.
    Variant B generates higher quality summaries that respect the document's subject, include concrete details, and offer a clear conclusion.
    Detailed prompts guide the model to prioritize relevant information according to the user's expectations.
    
    Observed limitations:
    1. PDF reading: PyMuPDF cannot read images or scanned documents - only PDFs with real text.
    2. Hallucinations: the model may add unverified information compared to the source text.
    3. Imprecise length: sometimes the 4-5 line constraint is not strictly followed.
    4. Each API call is independent; therefore context does not accumulate between calls.
    5. Free tier limits: the free plan offered by Google AI Studio has a restrictive maximum number of requests per minute/day.
"""

# Function execution

def main():
    print("PROCESSING PDF DOCUMENT TEXT WITH LLM")

    # Reading the PDF
    print(f'\n Step 0: READ PDF')
    try:
        SOURCE_TEXT = read_pdf(PDF_PATH)
    except FileNotFoundError as e:
        print(f'\nERROR: {e}')
        print("Make sure PDF_PATH points to the correct file")
        return
    except ValueError as e:
        print(f"\nERROR: {e}")
        print("Try a different PDF that contains selectable text")
        return

    print(f"\n Preview of extracted text (first 300 characters):")
    print(f"    {SOURCE_TEXT[:300]}[...]")

    # 1. Summary
    print("\n\n Step 1: Generate summary")
    summary, t1 = measure_time(generate_summary, SOURCE_TEXT)
    display_section(f"SHORT SUMMARY [time: {t1}s]", summary)

    # 2. Key information
    print("\n\n Step 2: Extract key information")
    information, t2 = measure_time(extract_information, SOURCE_TEXT)
    display_section(f"KEY INFORMATION [time: {t2}s]", information)

    # 3. Answers to questions
    print("\n\n Step 3: Answer questions")
    answers = []
    for i, question in enumerate(QUESTIONS, 1):
        answer, t = measure_time(answer_question, SOURCE_TEXT, question)
        answers.append({"question": question, "answer": answer})
        display_section(f"QUESTION {i} [time: {t}s]\n {question}", answer)

    # 4. Prompt comparison
    print("\n\n Step 4: Compare two prompt variants")
    comparison = compare_prompts(SOURCE_TEXT)
    display_section(f"VARIANT A - Minimal prompt [Time: {comparison['variant_a']['time']}s]", comparison['variant_a']['result'])

    display_section(f"VARIANT B - Structured prompt [Time: {comparison['variant_b']['time']}s]",
                      comparison['variant_b']['result'])

    # 5. Analysis
    display_section("Comparative analysis and conclusions", ANALYSIS)

    # Save results to JSON
    results = {
        "model": MODEL,
        "sdk": "google-genai",
        "pdf_source": PDF_PATH,
        "extracted_text_words": len(SOURCE_TEXT.split()),
        "summary": summary,
        "key_information": information,
        "question_answers": answers,
        "prompt_comparison": {
            "prompt_a": PROMPT_VARIANT_A[:100] + "...",
            "result_a": comparison["variant_a"]["result"],
            "time_a": comparison["variant_a"]["time"],
            "result_b": comparison["variant_b"]["result"],
            "time_b": comparison["variant_b"]["time"],
        },
        "analysis": ANALYSIS
    }

    with open("results.json", "w", encoding="utf-8") as f:
        json.dump(results, f, ensure_ascii=False, indent=2)

    print(f"\n\nResults saved to results.json")

if __name__ == "__main__":
    main()
