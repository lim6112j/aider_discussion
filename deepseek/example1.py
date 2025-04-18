import os
from openai import OpenAI

DEEPSEEK_API_KEY = os.getenv("DEEPSEEK_API_KEY")
client = OpenAI(api_key=DEEPSEEK_API_KEY,
                base_url="https://api.deepseek.com")

# Round 1
messages = [{"role": "user", "content": "9.11 and 9.8, which is greater?"}]
response = client.chat.completions.create(
    model="deepseek-reasoner",
    messages=messages,
    stream=True
)

reasoning_content = ""
content = ""

for chunk in response:
    if chunk.choices[0].delta.reasoning_content:
        reasoning_content += chunk.choices[0].delta.reasoning_content
    elif chunk.choices[0].delta.content:
        content += chunk.choices[0].delta.content
    else:
        content += ""
# Round 2
messages.append({"role": "assistant", "content": content})
messages.append(
    {'role': 'user', 'content': "How many Rs are there in the word 'strawberry'?"})
response = client.chat.completions.create(
    model="deepseek-reasoner",
    messages=messages,
    stream=True
)
new_content = ""
new_reasoning_content = ""
for chunk in response:
    if chunk.choices[0].delta.reasoning_content:
        new_reasoning_content += chunk.choices[0].delta.reasoning_content
    elif chunk.choices[0].delta.content:
        new_content += chunk.choices[0].delta.content
    else:
        new_content += ""
print(new_content)
print("----------------------------")
print(new_reasoning_content)
