def filtr(fun, lst):
    return [x for x in lst if fun(x)]

def foldl(fun, acc, lst):
    for x in lst:
        acc = fun(acc, x)
    return acc

def zipWith(fun, lst1, lst2):
    result = []
    min_len = (min(len(lst1), len(lst2)))
    
    for i in range(min_len):
        result.append(fun(lst1[i], lst2[i]))
        
    return result


def main():
    a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    b = [4, 7, 10]
    print("All even numbers in the range 1 to 10:")
    print(filtr(lambda x: x % 2 == 0, a))
    print("The product of the numbers 1 to 10:")
    print(foldl(lambda z, x: z * x, 1, a))
    print("Zipping two lists by multiplying their elements:")
    print(zipWith(lambda x, y: x * y, a, b)) 
    
if __name__ == "__main__":
    main()