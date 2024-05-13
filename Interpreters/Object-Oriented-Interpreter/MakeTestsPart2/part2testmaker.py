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

Test 1b

var x = 10;
{
  var y = 2;
  var z = x * y;
  x = z;
}
return x;
Test 2b

var a = 31160;
var b = 1476;
if (a < b) {
  var temp = a;
  a = b;
  b = temp;
}
var r = a % b;
while (r != 0) {
  a = b;
  b = r;
  r = a % b;
}
return b;

Test 3b

var x = 0;
var y = 10;
while (!(x >= y) || !(y > 25)) {
  x = x + 2;
  y = y + 1;
}
return x;
Test 4b

var x = 1;
var y = x + 1;
if (x < y) {
  var z = 10;

  if (x < z) {
    var swap = y;
    y = x;
    x = swap;
  }
}
return x;
Test 5b

var x = 10;
var y = 4;
if (x < y) {
  var min = x;
}
else {
  var min = y;
}
return min;
Test 6b

var x = 0;
x = x + 25;
return x;
x = x + 25;
return x;
x = x + 25;
return x;
Test 7b

var x = 0;
var result = 0;

while (x < 10) {
  if (result > 15) {
    return result;
  }
  result = result + x;
  x = x + 1;
}
return result;
Test 8b

var x = 0;
while (x < 6) {
  x = x + 1;
  continue;
  x = x + 100;
}
return x;

Test 9b

var x = 0;
while (x < 10) {
  x = x - 1;
  break;
  x = x + 100;
}
return x;
Test 10b

var x = 0;
var y = x;
var z = y;
while (1 == 1) {
  y = y - x;
  while (2 == 2) {
    z = z - y;
    while (3 == 3) {
      z = z + 1;
      if (z > 8)
        break;
      else
        continue;
    }
    y = y + 1;
    if (y <= 7)
      continue;
    else
      break;
  }
  x = x + 1;
  if (x > 6)
    break;
  else
    continue;
}
return x * 100 + y * 10 + z;
Test 11b

var x = 0;
while (x < 10) {
  var y = 0;
  x = x + 1;
  y = y - 1;
  break;
}
if (x > 0) {
  x = y;
}
return x;
Test 12b

var x = 1;
var y = 2;
if (x < y) {
  var z = 0;
  while (z < 100) {
    var a = 1;
    z = z + a;
    continue;
    z = 1000;
  }
  if (z != x) {
    z = a;
  }
}
return x;
Test 13b

var x = 1;
break;
return x;
Test 14b

var x = 1;
while (true) {
  x = x + 1;
  if (x > 10 && x % 2 == 0)
   break;
}
return x;
Test 15b


var x;

try {
  x = 20;
  if (x < 0)
    throw 10;
  x = x + 5;
}
catch(e) {
  x = e;
}
finally {
  x = x + 100;
}
return x;
Test 16b

var x;

try {
  x = 20;
  if (x > 10)
    throw 10;
  x = x + 5;
}
catch(e) {
  x = e;
}
finally {
  x = x + 100;
}
return x;
Test 17b

var x = 0;
var j = 1;

try {
  while (j >= 0) {
    var i = 10;
    while (i >= 0) {
      try {
        if (i == 0)
          throw 1000000;
        x = x + 10*i / i;
      }
      catch(e) {
        if (j == 0)
          throw 1000000;
        x = x + e / j;
      }
      i = i - 1;
    }
    j = j - 1;
  }
}
catch (e2) {
  x = x * 2;
}
return x;
Test 18b

var x = 10;
var result = 1;

try {
  while (x < 10000) {
     result = result - 1;
     x = x + 10;

     if (x > 1000) {
       throw x;
     }
     else if (x > 100) {
        break;
     }
  }
}
finally {
  result = result + x;
}
return result;
Test 19b

var x = 10;
var result = 1;

try {
  while (x < 10000) {
    result = result - 1;
    x = x * 10;

    if (x > 1000)
      throw x;
  }
}
catch (ex) {
  throw 1;
}
return result;
Test 20b

var x = 0;
while ((x = x + 1) < 21)
  x = x;
return x;
"""

create_test_files(input_text)
