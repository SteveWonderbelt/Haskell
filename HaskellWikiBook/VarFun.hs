--area = pi*r^2
r=5.0

x=5 --x is 5
y=6 --y is 6
--z = y --z is not defined
answer = 2 * {-
block comment, crossing line and...
-}3 {- inline comment. -} *7

area r = pi * r ^ 2

double x = 2*x
quadruple x = double(double x)
square x = x*x
half x = x/2
subtract12 x = half x-12

areaRect l w = l*w
areaTriangle b h = (b*h)/2

minus x y = x - y

volumeBox x y z = x*y*z

heron a b c = sqrt (s * (s-a) * (s-b) * (s-c))
      where
      s = (a+b+c)/2
areaTriangleTrig a b c = c * height/2 --basic trig
    where
    cosa = (b^2 + c^2 - a^2)/(2*b*c)
    sina = sqrt(1-cosa^2)
    height = b*sina
areaTriangleHeron a b c = result --use heron's 
    where
    result = sqrt(s*(s-a) * (s-b) - (s-c))
    s = (a+b+c)/2



