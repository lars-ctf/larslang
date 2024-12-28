import kotlinx.serialization.*
import kotlinx.serialization.json.*
import kotlinx.serialization.descriptors.*
import kotlinx.serialization.encoding.*
import me.any.*

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
                    jsonElement["sAddress"]!!.jsonObject["sAddress"]!!.let {
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
        println(jsonElement)
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
                return Branch(
                    BranchPolarity.valueOf(branch["polarity"]!!.jsonPrimitive.content),
                    jsonDecoder.json.decodeFromJsonElement<Address>(branch["address"]!!),
                    branch["label"]!!.jsonPrimitive.content
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

fun main() {
    val program = Json.decodeFromString<Program>(INPUT)
    println(transpileProgram(program))
}

const val C_RUNTIME = """
#include <stdio.h>

int main() {
    void* heap = malloc(1024);
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
            is Branch -> "if (heap[${transpileAccess(it.address)}] ${if (it.polarity == BranchPolarity.IFFALSE) "!" else ""}= 0) goto ${it.label};"
            else -> {
                throw IllegalArgumentException("Unknown instruction")
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
    return "&heap[${address.address}]"
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
