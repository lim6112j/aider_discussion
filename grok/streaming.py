import os
from openai import OpenAI

XAI_API_KEY = os.getenv("XAI_API_KEY")
client = OpenAI(
    api_key=XAI_API_KEY,
    base_url="https://api.x.ai/v1",
)

stream = client.chat.completions.create(
    model="grok-3-latest",
    messages=[
        {"role": "system", "content": "You are Grok, a chatbot inspired by the Hitchhikers Guide to the Galaxy."},
        {"role": "user", "content": "What is the meaning of life, the universe, and everything?"},
    ],
    stream=True  # Set streaming here
)

for chunk in stream:
    print(chunk.choices[0].delta.content, end="", flush=True)
