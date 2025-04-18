import os
from openai import OpenAI

# Retrieve API key from environment variable
XAI_API_KEY = os.getenv("XAI_API_KEY")

# Initialize the OpenAI client with xAI's base URL
client = OpenAI(base_url="https://api.x.ai/v1", api_key=XAI_API_KEY)

def generate_image(prompt):
    """Generate an image based on the provided prompt."""
    try:
        response = client.images.generate(
            model="grok-2-image",
            prompt=prompt,
            n=1  # Number of images to generate
        )
        image_url = response.data[0].url
        print(f"Generated image URL: {image_url}")
        return image_url
    except Exception as e:
        print(f"Error generating image: {e}")
        return None

def chat_mode():
    """Interactive chat mode for image generation."""
    print("Welcome to Image Generation Chat Mode!")
    print("Enter a description for the image you want to generate.")
    print("Type 'exit' or 'quit' to end the session.")
    
    while True:
        user_input = input("\nYour prompt: ").strip()
        
        if user_input.lower() in ['exit', 'quit']:
            print("Exiting chat mode. Goodbye!")
            break
            
        if not user_input:
            print("Please enter a valid prompt.")
            continue
            
        print(f"Generating image for: '{user_input}'")
        generate_image(user_input)

if __name__ == "__main__":
    chat_mode()
