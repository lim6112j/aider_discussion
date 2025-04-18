import os

from openai import OpenAI

XAI_API_KEY = os.getenv("XAI_API_KEY")
client = OpenAI(base_url="https://api.x.ai/v1", api_key=XAI_API_KEY)

response = client.images.generate(
    model="grok-2-image",
    prompt="A angry cute cat with white fur in manga style"
)

print(response.data[0].url)
