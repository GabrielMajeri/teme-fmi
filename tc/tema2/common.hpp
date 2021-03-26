#pragma once

#include "java-parser.hpp"

#define YY_DECL yy::parser::symbol_type yylex()
