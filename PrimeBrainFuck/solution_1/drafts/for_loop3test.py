a = [i for i in range(3, 1000, 2)][::-1]
for i in a:

    b = [j for j in range(i*3, 1000000, i*2)][::-1]
    for j in b:
        print(j)
