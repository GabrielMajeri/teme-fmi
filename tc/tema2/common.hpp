#pragma once

#define YY_DECL yy::parser::symbol_type yylex()

enum class AccessModifier
{
    DEFAULT,
    PUBLIC,
    PROTECTED,
    PRIVATE
};

enum class MutabilityModifier
{
    MUTABLE,
    FINAL
};

enum class StorageModifier
{
    INSTANCE,
    STATIC
};
