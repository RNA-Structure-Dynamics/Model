f=open('D:/BiologyBasic/Anal/cyN1.sam')
st=f.read()
Ls=st.split('\n')
for i in range(1,len(Ls)-1):
    st1=Ls[i].split('\t')
    Ls[i]=st1[3]

f.close()
