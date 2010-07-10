package com.smallcultfollowing.lathos;

import java.io.IOException;

public interface PageContent 
{
	public void renderInPage(Output out) throws IOException;
}
