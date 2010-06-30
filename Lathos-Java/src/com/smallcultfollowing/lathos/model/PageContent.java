package com.smallcultfollowing.lathos.model;

import java.io.IOException;

public interface PageContent 
{
	public void renderInPage(Output out) throws IOException;
}
