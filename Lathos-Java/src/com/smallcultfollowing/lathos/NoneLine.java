package com.smallcultfollowing.lathos;

import java.io.IOException;

public class NoneLine implements Line {
	
	public static final NoneLine Line = new NoneLine();

	@Override
	public void renderInLine(Output output) throws IOException {
	}

	@Override
	public void addContent(CustomOutput content) {
	}

	@Override
	public void addText(String text) {
	}

	@Override
	public void addNumber(Number num) {
	}

}
