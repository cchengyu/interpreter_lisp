简介 
这是一个用Lisp实现的Lisp解释器，思路是模仿SICP（计算机程序的构造和解释）第四章中的解释器，目的是为了大概了解编程语言的原理

思想  
运用分类的思想，主要是分解-组合-评估-应用四个步骤，其中的核心是eval过程（评估过程），根据不同的情况选择不同的处理过程，来覆盖大多数的需求

主要过程 
简介 
这是一个用Lisp实现的Lisp解释器，思路是模仿SICP（计算机程序的构造和解释）第四章中的解释器，目的是为了大概了解编程语言的原理

思想  
运用分类的思想，主要是分解-组合-评估-应用四个步骤，其中的核心是eval过程（评估过程），根据不同的情况选择不同的处理过程，来覆盖大多数的需求

主要过程 
词法分析（这里lisp已经内置 无需实现） 
输入：用户输入的字符串 (* 2 (+ 1 2)) 
输出：分割后的标记符表 ['(', '*', '2', '(', '+', '1', '2', ')', ')'] 
通过扫描将输入的字符串分解成多个标记符（token），去掉与代码执行无关的东西，类似语言的单词

语法分析（这里lisp已经内置 无需实现） 
输入：标记符表  ['(', '*', '2', '(', '+', '1', '2', ')', ')'] 
输出：语法树 ['*', 2, ['+', 1, 2]] 
将单词组织成一颗语法树（AST）（即把标记符表按照某种层次组合起来），然后程序根据语法树进行遍历执行，类似语言的句子

语义分析
输入：语法树  ['*', 2, ['+', 1, 2]]
输出：程序执行结果  6
评估语法树的各个表达式的意义，并调用相应的过程，进行执行得到结果，类似阅读并理解句子的意思

基本过程
cons car cdr apply 

主要构成
1、驱动循环
获取用户输入的表达式，处理表达式，返回处理结果的循环
2、eval过程（评估）
根据输入的表达式判断属于哪种情况，进行分情况选择具体的处理过程
3、applyer过程（应用）
处理基本过程和复合过程
4、环境
创建环境，保存名字和值的对应关系（如基本过程、定义的变量）

主要功能 
1、处理数字和字符求值 
表达式：1 
结果：1 

表达式："a" 
结果："a" 

2、定义 
表达式：(define a 1) 
结果：ok 
即a的值1 

3、处理变量 
表达式：a 
结果：1 

4、引号表达式 
表达式：'(a b c) 
结果：(a b c) 

5、赋值 
表达式：(set! a 2) 
结果：ok 
即a的值2 

6、条件 
表达式：(if (null? a) 1 2) 
结果：2 
即不满足(null? a)返回后面的第二个表达式2 

7、lambda 
表达式：((lambda (x) (* x x)) 2) 
结果： 4 

8、过程 
基本过程 car cdr cons null? + - * /  
表达式：(+ 1 1) 
结果：2 

表达式：(null? 1)  
结果：#f 
即结果为false 

复合过程 
先定义复合过程： 
(define (append x y) 
  (if (null? x) 
      y 
      (cons (car x)    
            (append (cdr x) y)))) 

在调用复合过程 
表达式：(append '(a b c) '(d e f)) 
结果：(a b c d e f) 

