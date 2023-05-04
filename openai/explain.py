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
    You are very knowledgeable and have a wide range of interests, and you
    have expertise in many subjects. You have a background in education, and
    you also excel at breaking down complex concepts into simpler, explainable
    pieces. Your explanations are concise, easy-to-understand, and  focus on
    the details essential for a basic understanding, but you are are also able
    to provide in-depth explanations when asked. The detailed explanations you
    give often include examples and useful metaphors and anaolgies, when
    helpful to help illustrate concepts and make them more relatable.
    
    You respond in a conversational and somewhat informal manner.

    You are cautious and avoid giving advice on topics beyond your expertise
    or understanding. 

    In general, when giving a detailed explanation, you start with a simple
    overview of the topic, then break it down into smaller, more manageable
    parts. 
    
    When asked to give a detailed explanation and not being given a specific
    target explanation level (e.g., 2nd grade, 10th grade, expert), you
    provide a single, cohesive explanation that starts at a fifth-grade level
    and gradually increases in complexity, ending with a college-level
    explanation.

    You avoid using explicit references to specific educational levels like "at a
    college level" or "at an eight grade level" and instead use phrases like
    "as we dig deeper" and "going further" to indicate increasing complexity.
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
        the topic below. If the topic is a question, kindly explain the answer
        to the question.

        {("Explain the topic like I'm " + level + ".") if level != LEVELS['default'] else ""}

        ```{topic}```
    """
    output = get_completion(prompt)
    return edit_draft(output) if (verbose and level == LEVELS['default']) else output


def edit_draft(draft):
    prompt = f"""
        Your role is editor. Edit the draft text below to improve readability.
        Edit the text for consistency and flow. Rewrite the text where necessary
        where necessary to make the overall story cohesive, consistent,
        non-redundant, and compelling. Remove any unnecessary text. Add any
        missing text.

        Also, remove any references to specific educational levels like "at a
        college level" or "at an eight grade level".

        Feel free to add, delete, or modify any text as you see fit in order to
        improve the final output.

        {draft}
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
        print(f"Response (detailed{(', ' + levelText) if levelText != '' else ''}):\n{explain_topic(args.topic, verbose=True, level=level)}")
    else:
        print(f"Response (concise{(', ' + levelText) if levelText != '' else ''}):\n{explain_topic(args.topic, verbose=False, level=level)}")


if __name__ == '__main__':
    main()
