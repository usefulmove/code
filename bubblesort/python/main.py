def main():
    o = [3,1,2,5,4]
    recheck = True

    while recheck:
        recheck = False

        for i in range(len(o) - 1):
            if o[i] > o[i + 1]:
                recheck = True
                o[i], o[i+1] = o[i+1], o[i] # swap
    
    print(f"{o}")


if __name__ == "__main__":
    main()
