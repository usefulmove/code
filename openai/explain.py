#!/home/dedmonds/repos/code/openai/venv/bin/python3

import openai
import os
import sys

from dotenv import load_dotenv, find_dotenv
_ = load_dotenv(find_dotenv())  # load local .env file into environment

openai.api_key = os.getenv('OPENAI_API_KEY')


def get_completion(prompt, model="gpt-3.5-turbo", temperature=0.2):
    messages = [{"role": "user", "content": prompt}]
    response = openai.ChatCompletion.create(
        model=model,
        messages=messages,
        temperature=temperature,
    )
    return response.choices[0].message["content"]


def explain_topic(topic):
    prompt = f"""
        Please provide a concise explanation for the topic below. If the topic
        is a question, kindly answer it in a concise, straightforward, and
        easy-to-understand manner.

        ```{topic}```
    """
    return get_completion(prompt)


def explain_topic_detailed(topic):
    prompt = f"""
        Please provide a detailed explanation for the topic below. If the topic
        is a question, kindly answer it in a detailed and straightforward manner.

        ```{topic}```
    """
    return get_completion(prompt)


if len(sys.argv) > 1:
    if sys.argv[1] == "-v" or sys.argv[1] == "--verbose":
        verbose = True
        topic = sys.argv[2]
    else:
        verbose = False
        topic = sys.argv[1]

    print(f"Topic: {topic}")

    if verbose:
        print(f"Response (detailed): {explain_topic_detailed(topic)}")
    else:
        print(f"Response (concise): {explain_topic(topic)}")
