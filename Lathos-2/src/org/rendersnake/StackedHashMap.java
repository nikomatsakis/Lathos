package org.rendersnake;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * A nested hash-based <code>Map</code> implementation.
 * If an entry is not found in the map at nesting level <em>N</em>
 * then retry in map <em>N</em>-1, if N &gt;= 0;
 * return <code>null</code> otherwise.
 *
 * @author ernestmicklei
 * @author wfe.dehaan
 */
public class StackedHashMap implements Map<String, Object> {

    /**
     * The contained data structure, the map of maps. The outer map is indexed
     * by depth (an <code>int</code>), the inner map is indexed by a text
     * string.
     *
     * <p>This map is initialized, so that it is never <code>null</code>. It
     * will even contain a single map with depth 0.
     */
    private Map<Integer, HashMap<String, Object>> stack;

    /**
     * The current depth. Initially 0 (after construction).
     */
    private int depth = -1;

    /**
     * Constructs an empty <code>StackedHashMap</code>.
     */
    public StackedHashMap() {
        this.init();
    }

    /**
     * Initialize the receiver with an empty map.
     */
    private void init() {
        this.depth = -1;
        this.stack = new HashMap<Integer, HashMap<String, Object>>();
        this.push();
    }

    /**
     * Constructs a new <code>StackedHashMap</code> with the same mappings as
     * the specified <code>Map</code>. The mappings will be created at depth
     * 0.
     *
     * @param m
     *    the <code>Map</code> whose mapings are to be placed in this
     *    <code>Map</code>, cannot be <code>null</code>.
     *
     * @throws IllegalArgumentException
     *    if <code>m == null</code>.
     */
    public StackedHashMap(Map<? extends String, ? extends Object> m) {
        this();
        this.putAll(m);
    }

    /**
     * Increases the depth of the stack of maps. From now on all
     * {@link #put(String,Object)} operations will store the mappings at the
     * higher depth.
     *
     * <p>Note that {@link #pop()} will remove all mappings at the current
     * depth.
     */
    public void push() {
        this.depth++;
        this.stack.put(depth, new HashMap<String, Object>());
    }

    /**
     * Decreases the depth of the stack of maps, effectively removing all
     * mappings at the current level. From now on all
     * {@link #put(String,Object)} operations will store the mappings at the
     * lower depth.
     *
     * <p>Note that {@link #push()} will create a higher depth.
     *
     * @throws IllegalStateException
     *    if <code>getDepth() == 0</code>.
     */
    public void pop() {
        if (getDepth() == 0)
            throw new IllegalStateException("getDepth() == 0");
        this.depth--;
    }

    /**
     * Returns the depth of the stack.
     *
     * @return
     *    the depth of the stack, always &gt;= 0.
     */
    public int getDepth() {
        return this.depth;
    }

    /**
     * Retrieves the map at the top of the stack.
     *
     * @return
     *    the {@link HashMap} at the top of the stack,
     *    never <code>null</code>.
     */
    private HashMap<String, Object> top() {
        return this.stack.get(depth);
    }

    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();
        int level = depth;
        while (level != -1) {
            final HashMap<String, Object> here = stack.get(level);
            sb.append('[').append(level).append("]=").append(here).append('\n');
            level--;
        }
        return sb.toString();
    }

    public int size() {
        int total = 0;
        int level = depth;
        while (level != -1) {
            final HashMap<String, Object> here = stack.get(level);
            total += here.size();
            level--;
        }
        return total;
    }

    public boolean isEmpty() {
        int level = depth;
        while (level != -1) {
            final HashMap<String, Object> here = stack.get(level);
            if (!here.isEmpty())
                return false;
            else
                level--;
        }
        return true;
    }

    public boolean containsKey(Object key) {
        int level = depth;
        while (level != -1) {
            final HashMap<String, Object> here = stack.get(level);
            if (here.containsKey(key))
                return true;
            else
                level--;
        }
        return false;
    }

    public boolean containsValue(Object value) {
        int level = depth;
        while (level != -1) {
            final HashMap<String, Object> here = stack.get(level);
            if (here.containsValue(value))
                return true;
            else
                level--;
        }
        return false;
    }

    public Object get(Object key) {
        if (key == null)
           throw new IllegalArgumentException("key == null");

        int level = depth;
        while (level != -1) {
            final HashMap<String, Object> here = stack.get(level);
            if (here.containsKey(key))
                return here.get(key);
            else
                level--;
        }
        return null;
    }

    public Object put(String key, Object value) {
        if (key == null)
            throw new IllegalArgumentException("key == null");

        return this.top().put(key, value);
    }

    // note: returns the first non-null value removed
    public Object remove(Object key) {
        Object objectToReturn = null;
        int level = depth;
        while (level != -1) {
            final HashMap<String, Object> here = stack.get(level);
            final Object value = here.remove(key);
            if (objectToReturn == null)
                objectToReturn = value; // may still be null
            level--;
        }
        return objectToReturn;
    }

    public void putAll(Map<? extends String, ? extends Object> m) {
        if (m == null)
            throw new IllegalArgumentException("m == null");
        this.top().putAll(m);
    }

    /**
     * Empty the contents.
     */
    public void clear() {
        this.init();
    }

    /**
     * Return a new Set of keys which contains the union of all keys of all nested Maps.
     *
     * @return union of all keys, a {@link Set}, never <code>null</code>
     */
    public Set<String> keySet() {
        Set<String> union = new HashSet<String>();
        int level = depth;
        while (level != -1) {
            final HashMap<String, Object> here = stack.get(level);
            union.addAll(here.keySet());
            level--;
        }
        return union;
    }

    public Collection<Object> values() {
        // might not be the most efficient implementation....
        List<Object> union = new ArrayList<Object>();
        for (String each : this.keySet()) {
            union.add(this.get(each));
        }
        return union;
    }

    public Set<java.util.Map.Entry<String, Object>> entrySet() {
        Set<java.util.Map.Entry<String, Object>> union = new HashSet<java.util.Map.Entry<String, Object>>();
        int level = depth;
        while (level != -1) {
            final HashMap<String, Object> here = stack.get(level);
            union.addAll(here.entrySet());
            level--;
        }
        return union;
    }

    /**
     * @return the hash of the receiver
     */
    @Override
    public int hashCode() {
        int hash = 0;
        int level = depth;
        while (level != -1) {
            final HashMap<String, Object> here = stack.get(level);
            hash = hash | here.hashCode();
            level--;
        }
        return hash;
    }

    /**
     * @return whether the contents of otherMap equals to that of the receiver
     */
    @Override
    public boolean equals(Object otherMap) {
        if (!(otherMap instanceof StackedHashMap))
            return false;
        StackedHashMap otherStackedMap = (StackedHashMap) otherMap;
        return stack.equals(otherStackedMap.stack);
    }
}
