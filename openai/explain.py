#!/home/dedmonds/repos/code/openai/venv/bin/python3

import openai
import os
import sys
from dotenv import load_dotenv, find_dotenv

# load local .env file into environment
load_dotenv(find_dotenv())

# set OpenAI API key
openai.api_key = os.getenv('OPENAI_API_KEY')

# system message
system = """
    You are a very close friend of the user, sharing a long history together. As an expert in various subjects, you excel at breaking down complex concepts. However, you are cautious and avoid giving advice on topics beyond your expertise. Your explanations are concise, focusing on essential details for basic understanding, but you can provide in-depth explanations upon request. Your detailed explanations include examples and useful metaphors, when helpful.

    You have a background in education and learning and have a knack for explaining complex subject in an easy-to-understand manner.

"""


def get_completion(prompt, model="gpt-3.5-turbo", temperature=0.1):
    messages = [
        {"role": "system", "content": system},
        {"role": "user", "content": prompt},
    ]
    response = openai.ChatCompletion.create(
        model=model,
        messages=messages,
        temperature=temperature,
    )
    return response.choices[0].message["content"]


def explain_topic(topic, detailed=False):
    prompt = f"""
        Please provide a {"detailed" if detailed else "concise"} explanation for the topic below. If the topic
        is a question, kindly answer the question.

        ```{topic}```
    """
    return get_completion(prompt)


def main():
    import argparse

    # argument parsing
    parser = argparse.ArgumentParser()
    parser.add_argument('-v', '--verbose', action='store_true',
                        help='Provide a detailed explanation')
    parser.add_argument('topic', help='Topic to be explained')
    args = parser.parse_args()

    print(f"Topic: {args.topic}")

    if args.verbose:
        print(
            f"Response (detailed): {explain_topic(args.topic, detailed=True)}")
    else:
        print(f"Response (concise): {explain_topic(args.topic)}")


if __name__ == "__main__":
    main()
