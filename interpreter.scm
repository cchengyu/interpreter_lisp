#lang sicp

; -----------------------评估----------------------------
; 评估
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((applications? exp)
         (applyer (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
	 (error "Unknown expression type --EVAL" exp))))

; 自求值表达式(数字和字符串)
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; 判断表达式是否为定义
(define (definition? exp)
  (tagged-list? exp 'define))

; 定义
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

; 取出定义的名字表达式
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

; 取出定义的过程表达式
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

; 构造lambda函数
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; 判断变量
(define (variable? exp) (symbol? exp))

; 定义变量
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

; 绑定变量和值到表里
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

; 寻找变量
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; 判断引号表达式
(define (quoted? exp) (tagged-list? exp 'quote))

; 取出引号表达式
(define (text-of-quotation exp) (cadr exp))

; 判断赋值
(define (assignment? exp) (tagged-list? exp 'set!))

; 赋值过程
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

; 设置变量值
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable --SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; 取出赋值的变量
(define (assignment-variable exp) (cadr exp))

; 取出赋值的值
(define (assignment-value exp) (caddr exp))

; 谓词检测
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

; 判断if表达式
(define (if? exp) (tagged-list? exp 'if))

; 判断表达式第一个是不是某个符号
(define (tagged-list? exp tag)
	(if (pair? exp)
		(eq? (car exp) tag)
		false))

; if表达式应用过程
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

; 提取if表达式表中第二个表达式
(define (if-predicate exp) (cadr exp))

; 提取if表达式表中第三个表达式
(define (if-consequent exp) (caddr exp))

; 提取if表达式中第四个表达式
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

; 判断lambda表达式
(define (lambda? exp)(tagged-list? exp 'lambda))

; 创建过程
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

; 取出lambda表达式的参数
(define (lambda-parameters exp) (cadr exp))

; 取出lambda表达式的body
(define (lambda-body exp) (cddr exp))

; 判断过程应用（不属于上诉各种表达式类型的任意复合表达式）
(define (applications? exp) (pair? exp))

; 实际应用基本过程(保存基础apply为另一个名字,避免重名)
;(define apply-in-underlying-scheme apply)


; -----------------------应用----------------------------
; 应用
(define (applyer procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

; 提取基本过程的操作
(define (operator exp) (car exp))

; 提取基本过程的参数表达式
(define (operands exp) (cdr exp))

; 判断是否没有参数
(define (no-operands? ops) (null? ops))

; 提取参数表中的第一个参数
(define (first-operand ops) (car ops))

; 提取参数表中除第一个参数外的剩余参数
(define (rest-operands ops) (cdr ops))

; 构造基本过程的参数表
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; 判断基本过程
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

; 应用基本过程
(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))


; 取出基本过程表达式
(define (primitive-implementation proc) (cadr proc))

; 判断复合过程
(define (compound-procedure? p)
  (tagged-list? p 'procedure))

; 求值复合过程
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; 判断是否最后一个表达式
(define (last-exp? seq) (null? (cdr seq)))

; 取出第一个表达式
(define (first-exp seq) (car seq))

; 取出第二个及以后的表达式
(define (rest-exps seq) (cdr seq))

; 取出复合过程的参数
(define (procedure-parameters p) (cadr p))

; 取出复合过程的过程体
(define (procedure-body p) (caddr p))

; 取出复合过程的环境
(define (procedure-environment p) (cadddr p))


; -----------------------环境----------------------------
; 设置环境
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

; 取出对应基本过程的名字
(define (primitive-procedure-names)
  (map car
       primitive-procedures))

; 得到基本过程和名字的对应关系的表
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        ))

; 得到基本过程和'primitive的表
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

; 扩展环境（环境就是表和外围环境的约束关系）
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

; 新建框架
(define (make-frame variables values)
  (cons variables values))

; 新建空环境
(define the-empty-environment '())

; 环境的外围环境
(define (enclosing-environment env) (cdr env))

; 环境的表
(define (first-frame env) (car env))

; 环境的表的变量
(define (frame-variables frame) (car frame))

; 环境的表的值
(define (frame-values frame) (cdr frame))


; ---------------------驱动循环--------------------------
; 定义输入提示符
(define input-prompt ";;; M-Eval input:")

; 定义打印输入提示符
(define (prompt-for-input string)
	(newline) (newline) (display string) (newline))
	; (newline) (newline) (display string))

; 定义打印输出提示符
;(define (announce-output string)
;	(newline) (display string) (newline))

; 定义输出提示符
(define output-prompt ";;; M-Eval value:")

; 定义打印结果（避免打印复合过程的环境部分）
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-precedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

; 定义驱动循环
(define (driver-loop)
	(prompt-for-input input-prompt)
	(let ((input (read)))
		(let ((output (eval input the-global-environment)))
			; (announce-output output-prompt)
			(user-print output)))
	(driver-loop))

; 初始化全局环境
(define the-global-environment (setup-environment))

; 启动驱动循环
(driver-loop)


#|
1、处理数字和字符求值
表达式:1
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
|#