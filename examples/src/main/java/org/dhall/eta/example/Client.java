package org.dhall.eta.example;

import org.dhall.eta.Input;

public class Client {

    public static void main (String[] args) {
        System.out.println("Testing dhall");
        System.out.println(Input.bool("True"));
        System.out.println(Input.bool("True && False"));
        try {
            System.out.println(Input.bool("1"));
        } catch (Exception e) {
            System.out.println(e.getLocalizedMessage());
        }
        System.out.println(Input.str("let str=\"dhall\" in \"Hello ${str}\""));
        System.out.println(Input.integer("+1234567"));
        System.out.println(Input.natural("2 * 3 + 4"));
        System.out.println("The end");
    }
    
}
