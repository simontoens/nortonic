def greeter(sentence):
    return "greetings " + sentence + "!"

def greet_rest_of_sentence(sentence, word):
  i = sentence.find(word) + len(word) + 1
  return None if i == -1 else greeter(sentence[i:])

print(greet_rest_of_sentence("hello world", "hello"))


