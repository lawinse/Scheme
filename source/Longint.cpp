#include "Longint.h"
int judge(char a[],int a1,char b[],int b1)
{
      int i;
      if(a1<b1)return -1;
      bool flag=false;
      if(a1==b1)
      {
        for(i=a1-1;i>=0;i--)
          if(a[i]>b[i])
              flag=true;
          else if (a[i]<b[i])
          {
           if(!flag) return -1;
          }
      }
      for(i=0;i<a1;i++)
      {
          a[i]=a[i]-b[i]+48;
          if((a[i]-'0')<0)
          {
            a[i]=a[i]+10;a[i+1]=a[i+1]-1;
          }
      }
    for(i=a1-1;i>=0;i--)
         if(a[i]!='0')
              return (i+1);
    return 0;
}

string div(string a,string b)
{
    int a_len,b_len,i,j;
    a_len=a.length();
    b_len=b.length();
    int max_len  = max(a_len, b_len);
    int *c;
    c = new int [max_len+10];
    memset(c,0,sizeof(int)*(max_len+10));
    char *x1,*x2;
    x1 = new char [max_len+10];
    x2 = new char [max_len+10];
    memset(x1,'0',sizeof(char)*(max_len+10));
    memset(x2,'0',sizeof(char)*(max_len+10));



    for(i=a_len-1,j=0;i>=0;i--)
        x1[j++]=a[i];
    for(i=b_len-1,j=0;i>=0;i--)
        x2[j++]=b[i];
    if(a_len<b_len) return "0";
    int temp_len=judge(x1,a_len,x2,b_len);
    if(temp_len<0)return "0";
    if(temp_len==0)return "1";
    c[0]++;
    int ntimes=temp_len-b_len;
    if(ntimes<0)
        return "1";
    else if(ntimes>0)
    {
        for(i=temp_len-1;i>=0;i--)
        if(i>=ntimes)
          x2[i]=x2[i-ntimes];
        else
           x2[i]='0';
    }
    b_len=temp_len;
    for(j=0;j<=ntimes;j++)
    {
     int ntemp;
     while((ntemp=judge(x1,temp_len,x2+j,b_len-j))>=0)
     {
       temp_len=ntemp;
       c[ntimes-j]++;
     }
    }
    for(i=0;i<max_len+10;i++)
       if(c[i]>=10)
       {
         c[i+1]+=c[i]/10;
         c[i]%=10;
       }
    int k=max_len+9;
    string res="";
    while(c[k]==0&&k>0)k--;
    for(i=k;i>=0;i--)
        res+=(c[i]+'0');
    delete [] c;
    delete [] x1;
    delete [] x2;

    return res;
}

string add(string a, string b)
{
    reverse(a.begin(),a.end());
    reverse(b.begin(),b.end());

    int a_len=a.length();
    int b_len=b.length();
    int res_len=max(a_len,b_len),max_len = res_len;
    int *c;
    c = new int [max_len+10];
    memset(c,0,sizeof(int)*(max_len+10));
    for (int i=0;i<a_len;++i) c[i]+=a[i]-'0';
    for (int i=0;i<b_len;++i) c[i]+=b[i]-'0';
    for (int i=0;i<res_len;++i)
    {
        c[i+1]+=c[i]/10;
        c[i]=c[i]%10;
    }
    string res= "";
    if (c[res_len]!=0) res += "1";
    for (int i=res_len-1;i>=0;--i) res += c[i]+'0';
    delete [] c;
    if (res == "") res ="0";
    return res;
}

string mul(string a,string b)
{
    reverse(a.begin(), a.end());
    reverse(b.begin(), b.end());
    int len_a=a.length();
    int len_b=b.length();
    int max_len = max(len_a,len_b);
    int *c;
    c = new int [2*max_len+10];
    memset(c,0,sizeof(int)*(2*max_len+10));
    for (int i = 0; i < len_a;++i)
    for (int j = 0; j < len_b;++j)
    c[j+i]+=(a[i]-'0')*(b[j]-'0');
    for (int i = 0; i < len_a+len_b-1; ++i)
    {
    c[i+1]+=c[i]/10;
    c[i]=c[i]%10;
    }
    bool flg=1;
    string res="";
    for (int i = 2*max_len+8;i>=0;--i)
    {
    if (c[i]==0&&flg) continue;
    else res+=('0'+c[i]),flg=0;
    }
    delete [] c;
    if (res == "") res = "0";
    return res;
}



string sub(string a, string b)
{
    reverse(a.begin(),a.end());
    reverse(b.begin(),b.end());

    int a_len=a.length();
    int b_len=b.length();
    int res_len=max(a_len,b_len),max_len = res_len;
    int *c;
    c = new int [max_len+10];
    memset(c,0,sizeof(int)*(max_len+10));
    for (int i=0;i<a_len;++i) c[i]+=a[i]-'0';
    for (int i=0;i<b_len;++i) c[i]-=b[i]-'0';
    for (int i=0;i<res_len;++i)
    {

        if (c[i]<0)
        c[i+1]--,c[i]+=10;
    }
    string res= "";
    bool flg=1;
    for (int i = max_len;i>=0;--i)
    {
        if (c[i] == 0&&flg) continue;
        else res+=('0'+c[i]),flg=0;
    }
    if (res == "") res = "0";
    delete [] c;
    return res;
}
