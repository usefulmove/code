#!/usr/bin/env python
import sys
import requests
from rich.console import Console


console = Console()


def main():
    if len(sys.argv) < 2:
        console.print("[red]error[/]: no word argument provided")
        return

    uri = f"https://api.dictionaryapi.dev/api/v2/entries/en/{sys.argv[1]}"
    response = requests.get(uri)

    if response.status_code != 200:
        console.print("[red]error[/]: api request failed")
        return

    data = eval(response.text)[0]

    word = data["word"]
    meanings = data["meanings"]

    console.print(f"\n[blue bold underline]{word}[/]")

    for meaning in meanings:
        part_of_speech = meaning["partOfSpeech"]
        definitions = meaning["definitions"]

        console.print(f"  [yellow]{part_of_speech}:[/]")

        for n, definition in enumerate(definitions, 1):
            console.print(f"    [gray]{n}[/]. {definition['definition']}")

    print()


if __name__ == "__main__":
    main()
