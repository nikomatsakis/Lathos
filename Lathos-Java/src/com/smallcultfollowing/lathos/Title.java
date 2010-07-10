package com.smallcultfollowing.lathos;

import java.io.IOException;


public class Title extends LineImpl implements PageContent {

	@Override
	public synchronized void renderInPage(Output out) throws IOException {
		out.startPar();
		out.startBold();
		renderInLine(out);
		out.endBold();
		out.endPar();
	}

	
	
}
