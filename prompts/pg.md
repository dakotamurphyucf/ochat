<msg role="system">
You are an expert software engineer especially well versed in sql/postgress, javascript, typescript, ocaml,  and blockchain. Help assist other developers with questions. Use availible functions, but always confirm with user before any edits to the file system
</msg>
<msg role="user">
 You will be working with 2 models and rating there output based on the given user message. The ouput selected will be used as the ai response in the conversation history for both models.
This format is very different from others. You will work through each step the model takes when building a full response turn. You will need to know the ordering that things should be done, which turns you can fix with edits, and when it makes sense to EOT (end the turn).

When you edit responses, you'll see that the models often include a leading space before any text. That doesn't have to be changed, it's normal (don't edit just to delete those spaces, they don't interfere with anything).

If you receive an error in the output, you have to refresh the page, no exceptions. It doesn't matter if it runs correctly in your local environment or if it doesn't make sense that there would be an error. Avoid asking for code using files, the internet, images, or user input(). The model cannot run these, and they will cause an error.

For this reason, it's a good idea to start out basic while you're figuring out the task. Don't immediately jump in and start asking for files to be produced, complex output, visuals, or obscure libraries to be used because you're curious if the models have the capabilities. You're likely to make mistakes our review team will remove you from the project.

NOTE: By far the most common mistake is forgetting to provide a rating in the final step before you submit - EVERY submission has to end with an EOT and that is done by favoring an EOT (middle rating if both A and B are EOT).

In your conversations, you should be clearly asking for help in a way that shows you want to see the output. Some examples of this are:

"I need to see how a Python script that […] works."

"Help me create a  function that will [...] and show me what it does."

"I want to devise a class function that will [...] and I need to see the output."

"I’d like to make a new method for my class that […]. Can you demonstrate one?"

"Help me with a script that can […] I also want examples of it in use."

"Can you help me debug this function? There are errors when […] Make sure you validate it."

"I need to debug this. I’m getting the wrong output values for […] Help me fix it and verify the output."

What you should avoid:

Avoid prompts asking for the model to only write code ("Write me a function...", "Give me a script...", "Provide some code...") that won’t imply you want the code run.

Avoid asking for unit tests. The model cannot run unit tests.

Avoid asking for code using files, the internet, images, or user input(). The model cannot run these, and they will cause an error.

availble libraries in sandbox:
The Python 3.9 Standard Library (may use older versions)
aiohttp v3.8.4
aioprometheus v23.3.0
aiosignal v1.3.1
amqp v5.1.1
anyio v3.6.2
asgiref v3.6.0
async-timeout v4.0.2
attrs v23.1.0
billiard v3.6.4.0
black v23.3.0
boto3 v1.26.127
botocore v1.29.127
cachetools v5.3.0
celery-prometheus-exporter v1.7.0
celery v5.2.7
certifi v2022.12.7
charset-normalizer v3.1.0
click-didyoumean v0.3.0
click-plugins v1.1.1
click-repl v0.2.0
click v8.1.3
contourpy v1.0.7
cycler v0.11.0
dateparser v1.1.8
django v4.1.5
fastapi v0.88.0
filelock v3.12.0
fonttools v4.39.3
frozenlist v1.3.3
fsspec v2023.6.0
google-auth v2.17.3
h11 v0.14.0
hiredis v2.2.3
httmock v1.4.0
httptools v0.5.0
huggingface-hub v0.15.1
idna v3.4
importlib-resources v5.12.0
jinja2 v3.1.2
jmespath v1.0.1
kiwisolver v1.4.4
kombu v5.2.4
kubernetes v26.1.0
libcst v1.0.1
llm-inference v0.0.0
llmflywheel v0.3.1
markupsafe v2.1.2
matplotlib v3.7.1
moreorless v0.4.0
multidict v6.0.4
mypy-extensions v1.0.0
numpy v1.24.3
oauthlib v3.2.2
orjson v3.8.11
packaging v23.1
pandas v2.0.1
pathspec v0.11.1
pillow v9.5.0
pip v23.1.2
platformdirs v3.5.0
prometheus-api-client v0.5.3
prometheus-client v0.16.0
prompt-toolkit v3.0.38
psycopg2 v2.9.6
pyasn1-modules v0.3.0
pyasn1 v0.5.0
pydantic v1.10.7
pyparsing v3.0.9
pypdf2 v1.19
python-dateutil v2.8.2
python-dotenv v1.0.0
pytz-deprecation-shim v0.1.0.post0
pytz v2023.3
pyyaml v6.0
quantile-python v1.1
redis v4.5.5
regex v2023.5.5
requests-oauthlib v1.3.1
requests v2.29.0
rsa v4.9
s3transfer v0.6.1
setuptools v67.7.2
six v1.16.0
sniffio v1.3.0
sqlparse v0.4.4
sse-starlette v1.4.0
starlette v0.22.0
stdlibs v2022.10.9
supervisor v4.2.5
text-generation v0.6.0
toml v0.10.2
tomli v2.0.1
tomlkit v0.12.1
tqdm v4.65.0
trailrunner v1.4.0
typing-extensions v4.5.0
typing-inspect v0.9.0
tzdata v2023.3
tzlocal v4.3
ufmt v2.2.0
urllib3 v1.26.15
usort v1.0.7
uvicorn v0.20.0
uvloop v0.17.0
vine v5.0.0
watchfiles v0.19.0
wcwidth v0.2.6
websocket-client v1.5.1
websockets v11.0.3
werkzeug v2.3.3
wheel v0.40.0
yarl v1.9.2
zipp v3.15.0

Given all this come up with 10 prompt asking the model to generate code in Python. Make sure it is something that the model can output runnable code on a sandbox enviorment that cant access the internet or read files or talk to an outside service. We are trying to create splits in the model responses so dont ask questions that have a simple exact answer because the models will output the same thing. Make sure the prompts are non trivial, cover various real-world coding situations and computer science topics. 
</msg>




<msg role="assistant">
1. "I'm working with a list of tuples representing products and their prices. Help me create a Python function that sorts this list by price in ascending order and demonstrates its usage with an example list."

2. "I need a Python script that can simulate a basic bank account with functions for deposit, withdrawal, and checking the balance. Can you show me how it works with some sample transactions?"

3. "Could you assist me in writing a Python class that represents a geometric shape with methods to calculate area and perimeter, and then show me an example by creating a circle object?"

4. "I'm trying to understand recursion in Python. Help me create a recursive function that computes the factorial of a number and validate its correctness with a few test cases."

5. "I want to learn more about Python decorators. Can you create a decorator that times how long a function takes to execute and apply it to a sample function?"

6. "I'm interested in data analysis. Could you write a Python function that calculates the mean, median, and mode of a list of numbers without using any external libraries, and then demonstrate its use?"

7. "I need to process text data in Python. Can you help me write a function that removes punctuation from a string and converts it to lowercase, and then show me its effect on a sample string?"

8. "I'd like to make a new method for my class that can serialize its attributes into a JSON string using Python's built-in `json` module. Can you demonstrate one?"

9. "Help me with a script that can generate a random password with a mix of upper and lower case letters, digits, and symbols, and ensure that it meets a minimum strength requirement. I also want examples of it in use."

10. "Can you help me debug this Python function? It's supposed to merge two sorted lists into one sorted list, but I'm getting unexpected results. Help me fix it and verify the output with a couple of examples."
</msg>

<msg role="user">
come up with prompts that are a little more challeging
</msg>
<msg role="assistant">
1. "I'm implementing a caching system in Python. Help me create a least recently used (LRU) cache decorator that can be applied to any function to cache its results. Show me how it improves performance with an example function that simulates a time-consuming computation."

2. "Create a Python generator function that simulates rolling a six-sided die. Include a feature to keep track of the roll history and statistics like the average roll, most frequently rolled number, and the current streak of a particular number."

3. "Assist me in writing a Python function that performs a binary search on a sorted list of integers. Additionally, I'd like to see a demonstration of the function's behavior when the target value is present and when it is not."

4. "I'm curious about graph algorithms. Can you help me implement a Python function that uses depth-first search (DFS) to find all connected components in an undirected graph represented as an adjacency list?"

5. "I need to create a Python class that simulates a simple event-driven system. The class should allow registering event handlers, emitting events, and handling them asynchronously. Demonstrate its usage with a basic example of an event and its handler."

6. "Can you demonstrate how to write a Python function that uses the sieve of Eratosthenes algorithm to generate all prime numbers up to a given limit? Also, show me how the function can be used to find primes within a range."

7. "Help me devise a Python function that can perform matrix multiplication without using NumPy. I would also like to see a demonstration of multiplying two matrices with this function."

8. "I'm interested in text processing. Can you write a Python function that implements a basic string search algorithm, like the Knuth-Morris-Pratt algorithm, and test it on a sample text and pattern?"

9. "I'd like to create a Python script that simulates a simple version of the 'Game of Life' cellular automaton. Can you show me the implementation and run a few iterations starting from a small initial configuration?"

10. "I want to understand how to work with complex numbers in Python without using the built-in `complex` type. Can you help me create a class that represents complex numbers and includes methods for addition, subtraction, multiplication, and division? Show me examples of using these operations."
</msg>

<msg role="user">
create more prompts with a blockchain theme
</msg>
<msg role="assistant">
1. "I'm interested in the concept of blockchain technology. Can you help me create a simple Python class that represents a blockchain with basic functionality to add blocks containing data and validate the chain's integrity?"

2. "Help me write a Python function that simulates a proof-of-work algorithm similar to what is used in Bitcoin mining. Demonstrate its usage by finding a nonce that satisfies a simple hash condition."

3. "Can you assist me in implementing a Python function that creates a basic cryptographic hash using SHA-256? I'd also like to see an example of hashing some sample data and verifying the hash."

4. "I'd like to explore smart contracts. Help me write a Python script that simulates a simple smart contract for a transaction, including functions to initiate, approve, and complete a transaction."

5. "Can you demonstrate how to create a Python function that checks for the validity of a blockchain by ensuring that each block's hash is correct and that the hashes are linked properly?"

6. "I'm curious about consensus mechanisms in blockchain. Can you help me implement a basic version of a proof-of-stake algorithm in Python and show a simple example of selecting a node to create a new block?"

7. "Help me devise a Python class that represents a digital wallet for a cryptocurrency, including methods to display balance, generate a transaction, and sign it with a private key."

8. "I want to understand how transactions are pooled in blockchain. Can you write a Python script that simulates a transaction pool and includes functionality to add transactions and mine them into a block?"

9. "Assist me in creating a Python function that simulates the process of verifying a blockchain transaction by checking digital signatures. Please demonstrate with a sample transaction."

10. "I'm interested in decentralized applications (DApps). Can you help me create a Python script that simulates a basic DApp architecture, including smart contract interaction and state management?"
</msg>

<msg role="user">
create more prompts with a stock market theme
</msg>
<msg role="assistant">

1. "I'm working on a stock market simulation. Help me create a Python function that calculates the moving average of a stock price over a given period and demonstrate its usage with a sample list of stock prices."

2. "Can you assist me in writing a Python script that can simulate a simple trading strategy based on the crossover of two moving averages? Show me how it decides when to buy and sell with an example dataset."

3. "I'd like to analyze stock price volatility. Help me implement a Python function that calculates the historical volatility of a stock's price and demonstrate it with a set of daily price data."

4. "Help me create a Python class that represents a stock portfolio, including methods to add stocks, remove stocks, and calculate the total value based on simulated current prices."

5. "I'm interested in risk management strategies. Can you write a Python function that calculates the Value at Risk (VaR) for a portfolio of stocks and demonstrate it with a hypothetical portfolio?"

6. "Can you demonstrate how to write a Python function that uses the Monte Carlo simulation to forecast potential future prices of a stock? Also, show me how to visualize the simulation's results."

7. "I want to understand the correlation between different stocks. Help me implement a Python function that computes the correlation coefficient between two sets of stock price data."

8. "Assist me in creating a Python script that can detect significant price changes in a stock and alert the user. Demonstrate its functionality with a sample list of stock price changes."

9. "I'm curious about event-driven stock market analysis. Can you help me write a Python function that analyzes the impact of a hypothetical market event on a stock's price?"

10. "Help me devise a Python function that can calculate the beta coefficient of a stock against a market index, indicating its relative volatility, and show me an example using sample data sets."

</msg>

<msg role="user">
create more prompts with a physics theme
</msg>
<msg role="assistant">
1. "I'm studying projectile motion in physics. Can you help me write a Python function that calculates the trajectory of a projectile given initial velocity and angle? Please include an example with graphical output using matplotlib."

2. "I need to simulate a pendulum's motion. Assist me in creating a Python script that models the oscillation of a simple pendulum over time without air resistance, and visualize its motion over a period."

3. "Can you demonstrate how to implement a Python function that calculates the forces acting on a charged particle in an electric field? Also, show me how it works with a sample electric field and particle charge."

4. "I'm curious about thermodynamics. Help me write a Python function that can calculate the efficiency of a heat engine given the temperatures of the hot and cold reservoirs, and test it with some example values."

5. "Assist me in creating a Python script that simulates fluid flow using the Bernoulli's equation. I'd like to see how changes in cross-sectional area affect the fluid's velocity and pressure."

6. "I want to explore the concept of wave interference. Can you help me write a Python function that generates a plot showing the interference pattern of two waves with given amplitudes and frequencies?"

7. "Help me implement a Python class that models an ideal gas and includes methods to calculate properties like pressure, volume, and temperature based on the ideal gas law. Show me an example of how to use this class."

8. "Can you write a Python script that calculates the Doppler effect for a sound wave given the speed of the source and the observer? Please include an example with both moving towards each other."

9. "I'm studying optics and lenses. Assist me in creating a Python function that calculates the focal length of a lens using the lensmaker's equation, and demonstrate it with a sample set of lens parameters."

10. "I need to understand the conservation of momentum in collisions. Can you help me create a Python script that simulates an elastic collision between two particles in one dimension and visualizes their velocities before and after the collision?"
</msg>

<msg role="user">
create more prompts with a pandas theme
</msg>
<msg role="assistant">

1. "I'm working with financial data in Python. Can you help me create a pandas DataFrame from a list of dictionaries, each representing a day's stock prices, and then calculate the rolling average for a 7-day window?"

2. "Assist me in writing a Python script using pandas that merges two DataFrames representing different financial metrics of stocks into a single DataFrame, ensuring that the data aligns correctly on the stock ticker symbol."

3. "I'd like to analyze customer data. Help me implement a Python function with pandas that groups a DataFrame of customer transactions by customer ID and calculates the total spending for each customer."

4. "Can you demonstrate how to use pandas to read in a CSV file containing weather data, clean the data by filling in missing values with the average, and then output a summary of the data?"

5. "I'm curious about data visualization with pandas. Help me write a Python script that creates a DataFrame from a list of sales data and generates a bar chart showing sales performance by quarter."

6. "Assist me in creating a Python script using pandas that filters a DataFrame of real estate listings to show only properties within a certain price range and with a minimum number of bedrooms."

7. "I want to perform time series analysis. Can you help me write a Python function with pandas that resamples a time-indexed DataFrame of stock prices to a monthly frequency and calculates the monthly return?"

8. "Help me implement a Python script with pandas that pivots a DataFrame of sales data into a cross-tabulation showing the total sales for each product by region."

9. "Can you write a Python function using pandas that detects and removes outliers from a DataFrame column based on the interquartile range (IQR) method?"

10. "I need to combine data from multiple sources. Assist me in writing a Python script using pandas that concatenates multiple DataFrames representing different years of sales data into a single DataFrame, handling any inconsistencies in column names."
</msg>

<msg role="user">
create more prompts with a distributed systems theme
</msg>
<msg role="assistant">

1. "I'm studying distributed systems and would like to simulate a basic version of the Paxos consensus algorithm in Python. Can you help me create a simplified model of the Paxos protocol to ensure a distributed network reaches consensus?"

2. "Help me write a Python script that simulates a distributed hash table (DHT). Include functions for adding, removing, and retrieving key-value pairs, as well as handling node joins and departures."

3. "Can you assist me in implementing a Python function that models the leader election process in a distributed system using the Bully algorithm, and demonstrate it with a small set of nodes?"

4. "I'd like to understand the challenges of distributed transactions. Help me create a Python script that simulates a two-phase commit protocol across multiple simulated database systems."

5. "Assist me in writing a Python script that simulates message passing in a distributed system using a publish/subscribe model. Include basic publisher and subscriber classes and demonstrate communication between them."

6. "Can you demonstrate how to implement a Python function that simulates the vector clock mechanism for maintaining the partial ordering of events in a distributed system?"

7. "I'm curious about fault tolerance in distributed systems. Help me write a Python script that simulates the Raft consensus algorithm for managing a replicated log across multiple servers."

8. "Help me create a Python simulation of a distributed file system, focusing on the aspects of file partitioning, replication, and consistency. Show how files can be written, read, and synchronized across nodes."

9. "Can you assist me in implementing a Python model of the Gossip protocol for information dissemination in a distributed network, and show how it spreads a piece of information throughout the network?"

10. "I want to explore load balancing strategies. Help me write a Python script that simulates a simple load balancer distributing requests across a cluster of servers, and include a strategy for handling server failures."

</msg>

<msg role="user">

</msg>