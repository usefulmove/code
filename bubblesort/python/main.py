def main():
    o = [3,1,2,5,4]

    recheck = True

    def swap(i, j):
        nonlocal recheck
        recheck = True

        tmp = o[i]
        o[i] = o[j]
        o[j] = tmp
    
    while recheck:
        recheck = False

        for i in range(len(o) - 1):
            if o[i] > o[i + 1]:
                swap(i, i + 1)
    
    print(f"{o}")


if __name__ == "__main__":
    main()
