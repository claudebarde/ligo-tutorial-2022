# Learn Ligo, a smart contract language for Tezos

![banner](https://ide.ligolang.org/logo.svg)

Ligo is one of the reasons that make me so passionate about Tezos. The first contract I ever wrote for Tezos was in Ligo. It's a language that was specially created for the Tezos blockchain and that is actively maintained and regularly improved. As a JavaScript developer, Ligo, and particularly CameLigo, may be your first contact with a functional language. And you will love it!

You will learn here _CameLigo_, one of the available syntaxes for Ligo that resembles the most OCaml. Although I started with _ReLigo_ (a syntax closer to ReasonML), I rapidly switched to CameLigo for 2 reasons: the first one, because most of the contracts you will find online are written either in PascaLigo or in CameLigo, so choosing a syntax that offers a lot of examples is a no-brainer. The second one, because I wanted to get more familiar with the syntax of OCaml in order to read Tezos source code. Although there is definitely a small learning curve when choosing CameLigo, it will give you a better understanding of OCaml code in general.

This tutorial is divided in 3 parts:

- In the first part, you will learn the basic concepts of Ligo to get you up and running and write smart contracts.
- In the second part, you will learn how to use the `Test` framework available with Ligo. It allows you to tests your contracts with the same language you wrote them in.
- In the third part, you will learn more advanced concepts of Ligo like modules, polymorphism and recursive functions when I show you how I built an assertion library for the `Test` framework.
