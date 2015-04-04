/**
 *   Copyright (C) 2013, 2015, Jozef Wagner. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 *   the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package dunaj.lang;

public class Util{

    static public boolean isInstance(Class c, Object o) {
        return c.isInstance(o);
    }

    static public boolean isInstance(Class c, byte b) {
        return (Byte.class == c) || (Byte.TYPE == c);
    }

    static public boolean isInstance(Class c, short b) {
        return (Short.class == c) || (Short.TYPE == c);
    }

    static public boolean isInstance(Class c, int b) {
        return (Integer.class == c) || (Integer.TYPE == c);
    }

    static public boolean isInstance(Class c, long b) {
        return (Long.class == c) || (Long.TYPE == c);
    }

    static public boolean isInstance(Class c, float b) {
        return (Float.class == c) || (Float.TYPE == c);
    }

    static public boolean isInstance(Class c, double b) {
        return (Double.class == c) || (Double.TYPE == c);
    }

    static public boolean isInstance(Class c, boolean b) {
        return (Boolean.class == c) || (Boolean.TYPE == c);
    }

    static public boolean isInstance(Class c, char b) {
        return (Character.class == c) || (Character.TYPE == c);
    }

}
