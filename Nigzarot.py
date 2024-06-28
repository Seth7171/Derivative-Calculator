import numpy as np

import time

# define efficient groups (sets)

# creation signs set
CREATION_SIGNS = {'+','-','*','/','^'}

# digits set
NUMBERS = {'0','1','2','3','4','5','6','7','8','9'}

# base set
B = {'sin(x)','cos(x)','tg(x)','arcsin(x)','arccos(x)','arctg(x)','exp(x)','ln(x)','x'}

# diff of base functions set
B_DIFF = {"cos(x)","-sin(x)","(1 / (cos(x) ** 2))","(1 / sqrt(1 - x ** 2))","(-1 / sqrt(1 - x ** 2))","(1 / (1 + x ** 2))","exp(x)","(1 / x)","0","1"}

B_DIFF_MAP = {"sin(x)": "cos(x)",
              "cos(x)": "-sin(x)",
              "tg(x)": "(1 / (cos(x) ** 2))",
              "arcsin(x)": "(1 / sqrt(1 - x ** 2))",
              "arccos(x)": "(-1 / sqrt(1 - x ** 2))",
              "arctg(x)": "(1 / (1 + x ** 2))",
              "exp(x)": "exp(x)",
              "ln(x)": "(1 / x)",
              "x": "1"}

B_EVAL_FUNCTIONS_MAP = {"sin(x)": np.sin,
                        "cos(x)": np.cos,
                        "tg(x)": np.tan,
                        "arcsin(x)": np.arcsin,
                        "arccos(x)": np.arccos,
                        "arctg(x)": np.arctan,
                        "exp(x)": np.exp,
                        "ln(x)": np.log,
                        "-sin(x)": lambda x: -np.sin(x),
                        "(1 / (cos(x) ** 2))": lambda x: 1 / (np.cos(x) ** 2),
                        "(1 / sqrt(1 - x ** 2))": lambda x: 1 / np.sqrt(1 - x ** 2),
                        "(1 / (1 + x ** 2))": lambda x: 1 / (1 + x ** 2),
                        "(1 / x)": lambda x: 1 / x,
                        "(2*x)": lambda x: 2*x,
                        "x": lambda x: x}

# define function that handle
# "-" sign evaluation
def eval_minus_handeling(f,x):
    if x!="not exists":
        return -f
    return lambda x: -f(x)

# define function that handle
# creation signs evaluation
def eval_creation_signs_handeling(f,g,sign,x):
    
    # values cases
    if x!="not exists":
        if sign == "+":
            return f+g
        if sign == "-":
            return f-g
        if sign == "*":
            return f*g
        if sign == "/":
            if g == 0:
                raise ValueError("Math Error")
            return f/g
        if sign == "^":
            if f == 0 and g == 0:
                raise ValueError("MathError")
            return f**g
        
    # functions cases
    if sign == "+":
        return lambda x: f(x)+g(x)
    if sign == "-":
        return lambda x: f(x)-g(x)
    if sign == "*":
        return lambda x: f(x)*g(x)
    if sign == "/":
        return lambda x: f(x)/g(x)
    if sign == "^":
        return lambda x: f(x)**g(x)

# define function that check
# if the string is string that
# present number (from R)
def is_number(string) -> bool:
    global NUMBERS
    if string[0] == '-':
        return is_number(string[1:])
    if string[0] == '.':
        return False
    dot_appeared = False
    for i in range(len(string)):
        if string[i] == ".":
            if dot_appeared:
             return False
            else:
             dot_appeared = True
        elif string[i] not in NUMBERS:
            return False
    return True

# define function that handle the case
# that the string present numbers (in Diff function)
def numbers_handeling(string_one,string_two,sign) -> str:
    if not is_number(string_one) or not is_number(string_two):
        return "(" + string_one + sign + string_two + ")"
    if sign == "+":
        return str(float(string_one)+float(string_two))
    if sign == "-":
        return str(float(string_one)-float(string_two))
    if sign == "*":
        return str(float(string_one)*float(string_two))
    if sign == "/":
        return str(float(string_one)/float(string_two))
    if sign == "^":
        return str(float(string_one)**float(string_two))

# define function that handle the case of
# 'adish' number (like 0 for + or 1 for *..)
def adish_handeling(string_one,string_two,adish) -> str:
    if string_one == adish:
        return string_two
    return string_one

# in ^ diff: handle g'(x)*ln(f(x)) part
def first_part_exp_d(dg_x,f_x) -> str:
    if is_number(f_x) and float(f_x) <= 0:
        raise ValueError("Math Error")
    if dg_x == "0":
        return "0"
    lan_f_x = str(np.log(float(f_x))) if is_number(f_x) else 'ln(' + f_x + ')'
    if dg_x == "1" or lan_f_x == "1":
        return adish_handeling(dg_x,lan_f_x,"1")
    return numbers_handeling(dg_x,lan_f_x,"*")

# in ^ diff: handle (g(x)*f'(x))/f(x) part
def second_part_exp_d(g_x,f_x,df_x):
    if f_x == "0":
        raise ValueError("Math Error")
    mone = adish_handeling(g_x,df_x,"1") if g_x == "1" or df_x == "1" else numbers_handeling(g_x,df_x,"*")
    if f_x == "1":
        return mone
    return numbers_handeling(mone,f_x,"/")

# define function that hendeling +
# or - creation signs (in Diff function)
def plus_or_minus_handeling(df_x,dg_x,sign) -> str:
    if df_x == "0" or dg_x == "0":
        return adish_handeling(df_x,dg_x,"0")
    return numbers_handeling(df_x,dg_x,sign)

# define function that hendeling *
# (in Diff function)
def multiply_handeling(f_x,g_x,df_x,dg_x,sign) -> str:
    if df_x == "0" or g_x == "0":
        if f_x == "0" or dg_x == "0":
            return "0";
        if f_x == "1" or dg_x == "1":
            return adish_handeling(f_x,dg_x,"1")
        return numbers_handeling(f_x,dg_x,"*")
    if f_x == "0" or dg_x == "0":
        if df_x == "1" or g_x == "1":
            return adish_handeling(df_x,g_x,"1")
        return numbers_handeling(df_x,g_x,"*")
    part_one = adish_handeling(df_x,g_x,"1") if df_x == "1" or g_x == "1" else numbers_handeling(df_x,g_x,"*")
    part_two = adish_handeling(f_x,dg_x,"1") if f_x == "1" or dg_x == "1" else numbers_handeling(f_x,dg_x,"*")
    return numbers_handeling(part_one,part_two,sign)

# define function that hendeling /
# (in Diff function)
def division_handeling(f_x,g_x,df_x,dg_x):
    if g_x == "0":
        raise ValueError("Math Error")
    mone = multiply_handeling(f_x,g_x,df_x,dg_x,"-")
    mechane = numbers_handeling(g_x,"2","^")
    return numbers_handeling(mone,mechane,"/")

# define function that hendeling ^
# (in Diff function)
def exponent_handeling(f_x,g_x,df_x,dg_x) -> str:
    if f_x == "0" and g_x == "0":
        raise ValueError("Math Error")
    if f_x == "0" or g_x == "0" or f_x == "1":
        return "0"
    if g_x == "1":
        return df_x
    start = numbers_handeling(f_x,g_x,"^")
    first_part_exp = first_part_exp_d(dg_x,f_x)
    second_part_exp = second_part_exp_d(g_x,f_x,df_x)
    end = numbers_handeling(first_part_exp,second_part_exp,"+")
    return numbers_handeling(start,end,"*")
    
# define function that return the
# diff of function (for example:
# Diff("(x^2)") = (2*x)
def Diff(f) -> str:
    
    # bases cases handeling
    if f in B_DIFF_MAP.keys():
        return B_DIFF_MAP[f]
    if is_number(f):
        return "0"
    if f[0] != '(' or f[-1] != ')':
        raise ValueError("Invalid function")
        
    # "-" case handeling
    string = f[1:-1]
    if string[0] == '-':
        return "(-" + Diff(string[1:]) + ")"
    
    # creation signs handeling
    brackets = 0
    for i in range(len(string)):
        if string[i] == '(':
            brackets += 1
        if string[i] == ')':
            brackets -= 1
        if brackets == 0 and (string[i] in CREATION_SIGNS):
            sign = string[i]
            f_x = string[:i]
            g_x = string[i+1:]
            if sign in {'+', '-'}:
                return plus_or_minus_handeling(Diff(f_x),Diff(g_x),sign)
            if sign == "*":
                return multiply_handeling(f_x,g_x,Diff(f_x),Diff(g_x),"+")
            if sign == "/":
                return division_handeling(f_x,g_x,Diff(f_x),Diff(g_x))
            if sign == "^":
                return exponent_handeling(f_x,g_x,Diff(f_x),Diff(g_x))
    raise ValueError("Invalid function")

# define function that evaluate the value
# function, or the value of the function
# for specific x in the function
# (if x is given)
def Eval(f, x="not exists"):
    
    # bases cases handeling
    if x == "not exists" and f in B_EVAL_FUNCTIONS_MAP.keys():
        return B_EVAL_FUNCTIONS_MAP[f]
    if f in B_EVAL_FUNCTIONS_MAP.keys():
        return B_EVAL_FUNCTIONS_MAP[f](x)
    if x == "not exists" and is_number(f):
        return lambda x: float(f)
    if is_number(f):
        return float(f)
    
    # "-" case handeling
    string = f[1:-1]
    if string[0] == '-':
        st = string[1:]
        if st not in B.union(B_DIFF) and (st[0] != '(' or st[-1] != ')'):
            st = '(' + st + ')'
        return eval_minus_handeling(Eval(st,x),x)
    
    # creation signs handeling
    brackets = 0
    for i in range(len(string)):
        if string[i] == '(':
            brackets += 1
        if string[i] == ')':
            brackets -= 1
        if brackets == 0 and (string[i] in CREATION_SIGNS):
            sign = string[i]
            f_x = string[:i]
            g_x = string[i+1:]
            return eval_creation_signs_handeling(Eval(f_x,x),Eval(g_x,x),sign,x)
    raise ValueError("Invalid function")
    
string = "x"
for i in range(8):
    string = "(" + string + "*" + string + ")"
 
before = time.time()
print(Diff(string))
print(Eval(Diff("(-(((x^ln(x))*sin(x))/(arccos(x)+(5*x))))"),0.5))
print(Eval(Diff(string))(1))
print(Eval(string,1))
T = time.time() - before

print("running time on Python: ",T)
