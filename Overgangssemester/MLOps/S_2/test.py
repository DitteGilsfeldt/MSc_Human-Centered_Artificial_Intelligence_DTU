# import requests
# response = requests.get('https://api.github.com/this-api-should-not-exist')
# print(response.status_code)

################################################

# import requests
# response = requests.get(
#     'https://api.github.com/search/repositories',
#     params={'q': 'requests+language:python'},
# )
# print(response.status_code)

# if response.status_code == 200:
#     print('Success!')
# elif response.status_code == 404:
#     print('Not Found.')

################################################

# import requests
# response = requests.get(
#     'https://api.github.com/search/repositories',
#     params={'q': 'requests+language:python'},
# )
# print(response.json())

# if response.status_code == 200:
#     print('Success!')
# elif response.status_code == 404:
#     print('Not Found.')

################################################


import requests
response = requests.get('https://imgs.xkcd.com/comics/making_progress.png')
print(response.status_code)

if response.status_code == 200:
    with open(r'img.png','wb') as f:
        f.write(response.content)
elif response.status_code == 404:
    print('Not Found.')
