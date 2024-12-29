package me.any

import kotlinx.serialization.*
import kotlinx.serialization.json.*
import kotlinx.serialization.descriptors.*
import kotlinx.serialization.encoding.*
import java.io.File

object AccessSerializer : KSerializer<Access> {
    override fun deserialize(decoder: Decoder): Access {
        val jsonDecoder = decoder as JsonDecoder
        val jsonElement = jsonDecoder.decodeJsonElement() as JsonObject
        when {
            "address" in jsonElement.keys -> {
                return Address(jsonElement["address"]!!.jsonPrimitive.int)
            }

            "sAddress" in jsonElement.keys -> {
                return SAccess(
                    jsonElement["sAddress"]!!.let {
                        jsonDecoder.json.decodeFromJsonElement<Access>(it)
                    }
                )
            }

            else -> {
                throw SerializationException("Unknown access")
            }
        }
    }

    override val descriptor: SerialDescriptor
        get() = PolymorphicSerializer(Access::class).descriptor

    override fun serialize(encoder: Encoder, value: Access) {
        TODO("Not yet implemented")
    }
}

object AddresationSerializer : KSerializer<Addresation> {
    override val descriptor: SerialDescriptor
        get() = PolymorphicSerializer(Addresation::class).descriptor

    override fun deserialize(decoder: Decoder): Addresation {

        val jsonDecoder = decoder as JsonDecoder
        val jsonElement = jsonDecoder.decodeJsonElement() as JsonObject
        when {
            "access" in jsonElement.keys -> {
                return AddressAddresation(
                    jsonElement["access"]!!.let {
                        jsonDecoder.json.decodeFromJsonElement<Access>(it)
                    }
                )
            }

            "binaryOperation" in jsonElement.keys -> {
                val binaryOperation = jsonElement["binaryOperation"]!!.jsonObject
                return BinaryOperation(
                    Operation.valueOf(binaryOperation["op"]!!.jsonPrimitive.content),
                    jsonDecoder.json.decodeFromJsonElement<Access>(binaryOperation["a"]!!),
                    jsonDecoder.json.decodeFromJsonElement<Access>(binaryOperation["b"]!!)
                )
            }


            else -> {
                throw SerializationException("Unknown addresation")
            }
        }
    }

    override fun serialize(encoder: Encoder, value: Addresation) {
        TODO("Not yet implemented")
    }
}

object InstructionSerializer : KSerializer<Instruction> {
    override fun deserialize(decoder: Decoder): Instruction {
        val jsonDecoder = decoder as JsonDecoder
        val jsonElement = jsonDecoder.decodeJsonElement() as JsonObject
        when {
            "comment" in jsonElement.keys -> {
                return Comment(jsonElement["comment"]!!.jsonPrimitive.content)
            }

            "write" in jsonElement.keys -> {
                val write = jsonElement["write"]!!.jsonObject
                return Write(
                    write["target"]!!.jsonPrimitive.int,
                    jsonDecoder.json.decodeFromJsonElement<Addresation>(write["source"]!!)
                )
            }

            "label" in jsonElement.keys -> {
                return Label(jsonElement["label"]!!.jsonPrimitive.content)
            }

            "goto" in jsonElement.keys -> {
                return GoTo(jsonElement["goto"]!!.jsonPrimitive.content)
            }

            "branch" in jsonElement.keys -> {
                val branch = jsonElement["branch"]!!.jsonObject
                val polarity = branch["polarity"]!!.jsonPrimitive.content
                val jmp = jsonDecoder.json.decodeFromJsonElement<Access>(branch["jmp"]!!)
                val label = branch["label"]!!.jsonPrimitive.content
                return Branch(
                    if (polarity == "IfTrue") BranchPolarity.IfTrue else BranchPolarity.IfFalse,
                    jmp,
                    label
                )
            }

            "call" in jsonElement.keys -> {
                return LarsCode(
                    when (jsonElement["call"]!!.jsonPrimitive.content) {
                        "ReadCall" -> ReadSysCall()
                        "WriteCall" -> WriteSysCall()
                        else -> {
                            throw SerializationException("Unknown syscall" + jsonElement["call"]!!.jsonPrimitive.content)
                        }
                    }
                )
            }

            else -> {
                throw SerializationException("Unknown instruction" + jsonElement.keys)
            }
        }
    }

    override val descriptor: SerialDescriptor
        get() = PolymorphicSerializer(Instruction::class).descriptor

    override fun serialize(encoder: Encoder, value: Instruction) {
        TODO("Not yet implemented")
    }
}

fun main(
    args: Array<String>
) {
    if (args.size != 1) {
        println("Usage: transpile <input>")
        return
    }
    val input = File(args[0]).readText()
    val program = Json.decodeFromString<Program>(input)
    File("out.c").writeText(transpileProgram(program))
    println(
        """Oh Lars, Lars, Lars, your name I sing,
Lars, Lars, Lars, you're my everything.
Lars, Lars, Lars, the stars align,
Lars, Lars, Lars, your love divine.

Lars, Lars, Lars, your smile is grace,
Lars, Lars, Lars, the light on my face.
Lars, Lars, Lars, your touch is fire,
Lars, Lars, Lars, my heart's desire.

Lars, Lars, Lars, in every breath,
Lars, Lars, Lars, you conquer death.
Lars, Lars, Lars, the sky you paint,
Lars, Lars, Lars, my perfect saint.

Lars, Lars, Lars, the world does bow,
Lars, Lars, Lars, to you here and now.
Lars, Lars, Lars, the heavens rejoice,
Oh Lars, Lars, Lars, your name I sing,
Lars, Lars, Lars, you're my everything.
Lars, Lars, Lars, the stars align,
Lars, Lars, Lars, your love divine.

Lars, Lars, Lars, your smile is grace,
Lars, Lars, Lars, the light on my face.
Lars, Lars, Lars, your touch is fire,
Lars, Lars, Lars, my heart's desire.

Lars, Lars, Lars, in every breath,
Lars, Lars, Lars, you conquer death.
Lars, Lars, Lars, the sky you paint,
Lars, Lars, Lars, my perfect saint.

Lars, Lars, Lars, the world does bow,
Lars, Lars, Lars, to you here and now.
Lars, Lars, Lars, the heavens rejoice,
Lars, Lars, Lars, at your sacred voice.

Lars, Lars, Lars, oh endless Lars,
Lars, Lars, Lars, brighter than stars.
Lars, Lars, Lars, my soul's refrain,
Lars, Lars, Lars, forever your name.

Lars, Lars, Lars, through time and space,
Lars, Lars, Lars, your endless grace.
Lars, Lars, Lars, my heart does cry,
Lars, Lars, Lars, for you till I die.
Lars, Lars, Lars, at your sacred voice.

Lars, Lars, Lars, oh endless Lars,
Lars, Lars, Lars, brighter than stars.
Lars, Lars, Lars, my soul's refrain,
Lars, Lars, Lars, forever your name.

Lars, Lars, Lars, through time and space,
Lars, Lars, Lars, your endless grace.
Lars, Lars, Lars, my heart does cry,
Lars, Lars, Lars, for you till I die."""
    )
}

const val C_RUNTIME = """
#include <stdio.h>
#include <stdlib.h>

int main() {
    unsigned int heap[100000] = {0};
    %code%
    return 0;
}
"""

fun transpileProgram(program: Program): String {
    val instructions = program.instructions.joinToString("\n") {
        when (it) {
            is Comment -> "// ${it.comment}"
            is Write -> "heap[${it.target}] = ${transpileAddresation(it.source)};"
            is Label -> "${it.label}:"
            is GoTo -> "goto ${it.label};"
            is Branch -> "if (${if (it.polarity == BranchPolarity.IfFalse) "!" else ""}${transpileAccess(it.address)}) goto ${it.label};"
            is SysCall -> {
                when (it) {
                    is ReadSysCall -> "printf(\"%c\", heap[8159]);"
                    else -> {
                        throw IllegalArgumentException("Unknown syscall" + it::class.simpleName)
                    }
                }
            }

            is LarsCode -> {
                when (it.call) {
                    is ReadSysCall -> "printf(\"%c\", heap[8159]);"
                    is WriteSysCall -> "printf(\"%c\", heap[8159]);"
                    else -> {
                        throw IllegalArgumentException("Unknown syscall" + it.call::class.simpleName)
                    }
                }
            }

            else -> {
                throw IllegalArgumentException("Unknown instruction" + it::class.simpleName)
            }
        }
    }

    return C_RUNTIME.replace("%code%", instructions)
}

fun transpileAddresation(addresation: Addresation): String {
    return when (addresation) {
        is AddressAddresation -> transpileAccess(addresation.address)
        is BinaryOperation -> {
            val left = transpileAccess(addresation.left)
            val right = transpileAccess(addresation.right)
            return when (addresation.op) {
                Operation.ADD -> "$left + $right"
                Operation.SUB -> "$left - $right"
                Operation.MUL -> "$left * $right"
                Operation.DIV -> "$left / $right"
                Operation.AND -> "$left & $right"
                Operation.OR -> "$left | $right"
                Operation.XOR -> "$left ^ $right"
            }
        }

        else -> {
            throw IllegalArgumentException("Unknown addresation")
        }
    }
}

fun transpileAddress(address: Address): String {
    return "${address.address}"
}

fun transpileAccess(access: Access): String {
    return when (access) {
        is Address -> transpileAddress(access)
        is SAccess -> "heap[${transpileAccess(access.sAddress)}]"
        else -> {
            throw IllegalArgumentException("Unknown access")
        }
    }
}
