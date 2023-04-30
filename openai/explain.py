#!/home/dedmonds/repos/code/openai/venv/bin/python3

import openai
import os
import sys

from dotenv import load_dotenv, find_dotenv
_ = load_dotenv(find_dotenv()) # read local .env file

openai.api_key = os.getenv('OPENAI_API_KEY')

def get_completion(prompt, model="gpt-3.5-turbo", temperature=0): 
    messages = [{"role": "user", "content": prompt}]
    response = openai.ChatCompletion.create(
        model=model,
        messages=messages,
        temperature=temperature, 
    )
    return response.choices[0].message["content"]

if len(sys.argv) > 1:
    prompt = sys.argv[1]
    print(f"{sys.argv[1]}")
    print(f"{get_completion(prompt)}")