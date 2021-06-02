## C-Compiler

**Development Schedule:**

| Timeline  | Content |
| --------- | ------- |
| 2021/5/30 |         |
| 2021/5/31 |         |

### Build Environment

This project is built upon *Ubuntu 18.04* ( LLVM on Windows requires complicated configuration )

##### (1) Flex & Bison

```sh
sudo apt install flex bison
```

##### (2) LLVM 9.0.0

- Download LLVM from official website: 
  - Website address: https://releases.llvm.org/download.html#9.0.0
  - Target Prebuilt Binaries: *clang+llvm-9.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz*
  - You can download the packet at host machine and drag it into virtual machine due to speed issue.

- Extract the tar.xz: 

```sh
tar xvf clang+llvm-9.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz
```

- Install Prerequisites: 

```sh
sudo apt install -y cmake zlib1g-dev libedit-dev libxml2-dev
```

- Suppose your LLVM 9.0.0 is extracted to `your_path/`,  then: 

```sh
cd your_path/
mv clang+llvm-9.0.0-x86_64-linux-gnu-ubuntu-14.04 llvm
sudo mv llvm /usr/local/llvm
```

- Set the Environment Variables: 

```sh
export LLVM_HOME=/usr/local/llvm/bin
export PATH=$LLVM_HOME:$PATH:/usr/local/llvm/lib:/usr/local/llvm/include
```

### Build Command

```sh
bison -d -o grammar.cpp grammar.y
flex -o token.cpp token.l
g++ `llvm-config --cppflags` -std=gnu++11 ./main.cpp ./token.cpp ./grammar.cpp -o parser `llvm-config --ldflags --libs` -ly
```

A Makefile will be provided later.

### Trouble Shooting

- union type should not contain shared_ptr<> type in grammar.y. (https://www.igoodtv.com/p/1742753.html)
- "'yylloc' was not declared in this scope" (http://www.cppblog.com/woaidongmao/archive/2008/11/23/67635.html)
- "‘fileno’ was not declared in this scope" (https://stackoverflow.com/questions/29692136/)
- "g++ compiler is not recognizing inbuilt input() function of lex" (https://stackoverflow.com/questions/32220319/)
- Undefined reference to 'yylex()' (https://stackoverflow.com/questions/1480138/)
- Undefined reference to 'vtable' (https://stackoverflow.com/questions/3065154/)





