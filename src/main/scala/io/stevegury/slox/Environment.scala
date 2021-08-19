package io.stevegury.slox

class Environment(enclosing: Environment = null) {
    private[this] val values: collection.mutable.Map[String, Any] = collection.mutable.HashMap.empty[String, Any]

    def get(name: Token): Any = {
        values.get(name.lexeme) match {
            case Some(value) => return value
            case _ if enclosing != null => enclosing.get(name)
            case _ => throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
        }
    }

    def define(name: String, value: Any): Unit = {
        values.put(name, value)
    }

    def assign(token: Token, value: Any): Unit = {
        if (values.contains(token.lexeme)) {
            values.put(token.lexeme, value)
            return
        } else if (enclosing != null) {
            enclosing.assign(token, value)
            return
        }
        throw new RuntimeError(token, s"Undefined variable '${token.lexeme}'.")
    }
}
