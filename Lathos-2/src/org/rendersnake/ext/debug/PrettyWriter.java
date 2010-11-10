package org.rendersnake.ext.debug;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

public class PrettyWriter extends Writer {

    public Writer writer;
    public int indentLevel = 0;
    private boolean inString = false;

    public PrettyWriter() {
        super();
        this.writer = new StringWriter(1024);
    }    
    public PrettyWriter(Writer wrapped) {
        super();
        this.writer = wrapped;
    }
    
    @Override
    public void write(char[] cbuf, int off, int len) throws IOException {
        if (len < 2) {
            writer.write(cbuf, off, len);
            return;
        }
        if (cbuf[off] == '<') {
            // open or close
            if (cbuf[off + 1] == '/') {
                // close
                indentLevel--;
                this.doIndent(indentLevel);
                writer.write(cbuf, off, len);
                writer.write('\n');
                return;
            } else if (cbuf[off + len - 2] == '/') {
                // empty
                this.doIndent(indentLevel);
                writer.write(cbuf, off, len);
                writer.write('\n');
                return;
            } else if (cbuf[off + len - 1] == '>') {
                // end open 
                this.doIndent(indentLevel);
                indentLevel++;
                writer.write(cbuf, off, len);
                this.checkQuotes(cbuf, len);
                this.doNewLine();
                return;                
            } else {
                // open open
                this.doIndent(indentLevel);
                indentLevel++;
                writer.write(cbuf, off, len);
                return;
            }
        }
        this.checkQuotes(cbuf, len);
        writer.write(cbuf, off, len);
        this.doNewLine();
    }

    private void doIndent(int level) throws IOException {
        if (inString)
            return;
        for (int i = 0; i < level; i++) {
            writer.write('\t');
        }
    }

    private void doNewLine() throws IOException {
        if (!inString)
            writer.write('\n');
    }

    private void checkQuotes(char[] cbuf, int len) {
        // count number of quotes
        int quotes = 0;
        for (int i = 0; i < len; i++) {
            quotes += cbuf[i] == '"' ? 1 : 0;
        }
        inString = quotes % 2 == 1;
    }

    @Override
    public void flush() throws IOException {
        writer.flush();

    }

    @Override
    public void close() throws IOException {
        writer.close();

    }

    public String toString() {
        return writer.toString();
    }
}
