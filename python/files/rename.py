import os

#print(dir(os))

os.chdir('/home/dedmonds/Desktop/test/')
print(os.getcwd())

filenames = os.listdir()


for index, file in enumerate(filenames):
    if file.endswith('.jpg') and len(file) > 10:
        #print(file)
        #print(f'{index:08}.jpg')
        os.rename(file, f'{index:08}.jpg')