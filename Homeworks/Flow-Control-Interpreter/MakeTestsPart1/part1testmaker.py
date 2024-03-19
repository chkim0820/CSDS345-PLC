def create_test_files(input_text):
    tests = input_text.split('Test ')
    for test in tests:
        if not test.strip():
            continue
        
        lines = test.split('\n')
        test_name = lines[0].strip().replace(':', '')
        code = '\n'.join(lines[1:]).strip()
        
        with open(f'test{test_name}.txt', 'w') as f:
            f.write(code)
        print(f'Created test{test_name}.txt')


input_text = """
Test 1a

return 150;
Test 2a

return 6 * (8 + (5 % 3)) / 11 - 9;
Test 3.

var z;
z = 10;
return z;
Test 4a

var x = (5 * 7 - 3) / 2;
return x;
Test 5a

var x = 10;
var y = 12 + x;
return x * y;
Test 6a

var x = 5;
var y = 6;
var m;
if (x <= y)
  m = x;
else
  m = y;
return m;
Test 7a

var x = 5;
var y = 6;
var m;
if (x >= y)
  m = x;
else
  m = y;
return m;
Test 8a

var x = 5;
var y = 6;
if (x != y)
  x = 10;
return x;
Test 9a

var x = 5;
var y = 6;
if (x == y)
  x = 10;
return x;
Test 10a

return 6 * -(4 * 2) + 9;
Test 11a

var x = 1;
y = 10 + x;
return y;
Test 12a

var y;
y = x;
return y;
Test 13a

var x;
var y;
x = x + y;
return x;
Test 14a

var x = 10;
var y = 20;
var x = x + y;
return x;
Test 15a

return (10 > 20) || (5 - 6 < 10) && true;
Test 16a

var x = 10;
var y = 20;
if (x < y && (x % 2) == 0)
  return 100;
else
  return 200;
Test 17a

var x = 100 % 2 == 0;
var y = 10 >= 20;
var z;
if (x || y)
  z = y;
else
  z = x;
return z;
Test 18a

var x = 10;
var y = 20;
var z = 20 >= 10;
if (!z || false)
  z = !z;
else
  z = z;
return z;
Test 19a

var x = 2;
while (x < 100)
  x = x * 2;
return x;
Test 20a

var x = 20;
var y = 128;
while (x * x > 128)
  x = x - 1;
x = x + 1;
return x;
"""

create_test_files(input_text)
