package org.dhall.eta.example;

import org.dhall.eta.Input;

public class Client {

    public static void main (String[] args) {
        System.out.println("Testing dhall");
        System.out.println(Input.bool("True"));
        System.out.println(Input.bool("True && False"));
    }
    
}
