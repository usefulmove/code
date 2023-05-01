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
    You are a very close friend of the user, sharing a long history together.
    As an expert in various subjects, you excel at breaking down complex
    concepts. However, you are cautious and avoid giving advice on topics
    beyond your expertise. Your explanations are concise, focusing on
    essential details for basic understanding, but you can provide in-depth
    explanations upon request. Your detailed explanations include examples
    and useful metaphors, when helpful.

    You have a background in education and learning and have a knack for
    explaining complex topics in a very easy-to-understand manner, and you
    respond in a conversational and somewhat informal manner.
"""

LEVELS = {
    'default': '',
    'second': 'a second grader',
    'fifth': 'a fifth grader',
    'eighth': 'a eighth grader',
    'tenth': 'a tenth grader',
    'expert': 'an expert in the field',
}


def get_completion(prompt, model='gpt-3.5-turbo', temperature=0.05):
    messages = [
        {'role': 'system', 'content': system},
        {'role': 'user', 'content': prompt},
    ]
    response = openai.ChatCompletion.create(
        model=model,
        messages=messages,
        temperature=temperature,
    )
    return response.choices[0].message['content']


def explain_topic(topic, verbose=False, level=LEVELS['default']):
    prompt = f"""
        Please provide a {'detailed' if verbose else 'concise'} explanation for
        the topic below. If the topic is a question, kindly answer the question.

        {("Explain the topic like I'm " + level + ".") if level != LEVELS['default'] else ""}

        ```{topic}```
    """
    return get_completion(prompt)


def main():
    import argparse

    # argument parsing
    parser = argparse.ArgumentParser()
    parser.add_argument('topic', help='topic to be explained')
    parser.add_argument('-v', '--verbose', action='store_true', help='provide a detailed explanation')
    parser.add_argument('-s', '--simple', action='store_true', help='provide a simple explanation')
    parser.add_argument('-2', '--second', action='store_true', help='provide an explanation suitable for a 2nd grader')
    parser.add_argument('-5', '--fifth', action='store_true', help='provide an explanation suitable for a 5th grader')
    parser.add_argument('-8', '--eighth', action='store_true', help='provide an explanation suitable for a 8th grader')
    parser.add_argument('-10', '--tenth', action='store_true', help='provide an explanation suitable for a 10th grader')
    parser.add_argument('-e', '--expert', action='store_true', help='provide an explanation suitable for an expert in the field')
    args = parser.parse_args()

    print(f'Topic: {args.topic}')

    if args.simple:
        level = LEVELS['eighth']
        levelText = '8th'
    elif args.second:
        level = LEVELS['second']
        levelText = '2nd'
    elif args.fifth:
        level = LEVELS['fifth']
        levelText = '5th'
    elif args.eighth:
        level = LEVELS['eighth']
        levelText = '8th'
    elif args.tenth:
        level = LEVELS['tenth']
        levelText = '10th'
    elif args.expert:
        level = LEVELS['expert']
        levelText = 'expert'
    else:   
        level = LEVELS['default']
        levelText = ''

    if args.verbose:
        print(f"Response (verbose{(', ' + levelText) if levelText != '' else ''}): {explain_topic(args.topic, verbose=True, level=level)}")
    else:
        print(f"Response (concise{(', ' + levelText) if levelText != '' else ''}): {explain_topic(args.topic, verbose=False, level=level)}")


if __name__ == '__main__':
    main()
