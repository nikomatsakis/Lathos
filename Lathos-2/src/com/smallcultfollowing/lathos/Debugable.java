package com.smallcultfollowing.lathos;

import java.io.IOException;

public interface Debugable
{
    public void renderAsLine(Output out, Link link) throws IOException;
}
