package me.any




const val INPUT = """
    {
    "instructions": [{ "comment": " erster kommentar" },{ "comment": " zweite Kommentar" },{ "comment": " dritter Kommentar" },{ "label": "lars" },{ "goto": "lars" },{ "comment": "/sral 0 @lars" },{ "write": { "target": 0, "source": { "access": { "address": 10 } } }},{ "write": { "target": 0, "source": { "binaryOperation": { "a": { "sAddress": { "sAddress": { "address": 10 } } }, "op": "ADD", "b": { "address": 5 } }} }}]
    
}

"""