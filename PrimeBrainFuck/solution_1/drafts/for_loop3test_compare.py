import time

while True:
    test_res_file = open("test_res.txt", "r")
    res_file = open("res.txt", "r")

    test_res = test_res_file.readlines()
    res = res_file.readlines()

    test_res_file.close()
    res_file.close()

    max_length = max([len(test_res), len(res)])
    err_count = 0

    try:
        for i in range(max_length):
            x = int(test_res[i])
            y = int(res[i])

            if x!=y:
                err_count+=1
            print("%s %s %s" % (x, y, x==y))
    except IndexError:
        pass

    print(err_count)
    if err_count != 0:
        exit()

    time.sleep(5)
