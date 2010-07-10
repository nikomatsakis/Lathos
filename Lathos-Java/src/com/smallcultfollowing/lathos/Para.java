package com.smallcultfollowing.lathos;

import java.io.IOException;


public class Para extends LineImpl implements PageContent {

	@Override
	public synchronized void renderInPage(Output out) throws IOException {
		out.startPar();
		renderInLine(out);
		out.endPar();
	}
	
}
