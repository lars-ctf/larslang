package me.any

import AccessSerializer
import AddresationSerializer
import InstructionSerializer
import kotlinx.serialization.Serializable

@Serializable(with = AccessSerializer::class)
interface Access

@Serializable
data class SAccess(val sAddress: Access) : Access

@Serializable
data class Address(val address: Int) : Access


enum class Operation {
    ADD, SUB, MUL, DIV, AND, OR, XOR
}

@Serializable(with = AddresationSerializer::class)
interface Addresation

data class AddressAddresation(val address: Access) : Addresation


data class BinaryOperation(
    val op: Operation,
    val left: Access,
    val right: Access
) : Addresation

interface SysCall

class WriteSysCall : SysCall
class ReadSysCall : SysCall

enum class BranchPolarity {
    IFTRUE, IFFALSE
}

@Serializable(with = InstructionSerializer::class)
interface Instruction

@Serializable
data class Comment(val comment: String = "") : Instruction

@Serializable
data class Write(val target: Int, val source: Addresation) : Instruction

@Serializable
data class LarsCode(val call: SysCall) : Instruction

@Serializable
data class Label(val label: String) : Instruction

@Serializable
data class GoTo(val label: String) : Instruction

@Serializable
data class Branch(val polarity: BranchPolarity, val address: Address, val label: String) : Instruction

@Serializable
data class Program(val instructions: List<Instruction>)