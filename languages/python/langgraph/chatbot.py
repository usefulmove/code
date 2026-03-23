import os
from anthropic import Anthropic
from rich.console import Console
from rich.markdown import Markdown


def main():
    console = Console()

    prompt = "Toni: How can I help you?"
    console.print(prompt)

    msg_history = [
        {
            "role": "assistant",
            "content": prompt,
        }
    ]

    while True:
        query = input("Duane: ")

        if query == "exit":
            console.print("Toni: Closing session. Have a great day!")
            break

        msg_history.append(
            {
                "role": "user",
                "content": query,
            }
        )

        client = Anthropic(api_key=os.environ.get("ANTHROPIC_API_KEY"))

        message = client.messages.create(
            max_tokens=1024,
            messages=msg_history,
            model="claude-sonnet-4-6",
        )

        response = message.content[0].text

        print("Toni:\n")
        console.print(Markdown(response))

        msg_history.append(
            {
                "role": "assistant",
                "content": response,
            }
        )


if __name__ == "__main__":
    main()
