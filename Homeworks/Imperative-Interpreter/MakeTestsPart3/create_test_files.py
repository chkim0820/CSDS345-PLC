import re

# Read the test cases from the file
with open("part3tests.html", "r") as file:
    input_string = file.read()

# Split the input string into individual test cases
tests = re.findall(r'<p>Test \d+: .*?<pre>.*?</pre>', input_string, re.DOTALL)

# Iterate over each test case and create a separate file for it
for i, test in enumerate(tests, start=1):
    # Extract the test number and code snippet
    test_number = re.search(r'Test (\d+):', test).group(1)
    code_snippet = re.search(r'<pre>(.*?)</pre>', test, re.DOTALL).group(1).strip()
    
    # Write the code snippet to a separate file
    with open(f"test{test_number}c.txt", "w") as file:
        file.write(code_snippet)

    print(f"Test {test_number} file created successfully.")
