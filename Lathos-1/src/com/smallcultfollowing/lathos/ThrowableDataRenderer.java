package com.smallcultfollowing.lathos;

import java.io.IOException;

public class ThrowableDataRenderer implements DataRenderer {
	
	class ThrowablePage implements Page {
		
		private final Throwable thr;
		
		public ThrowablePage(Throwable thr) {
			this.thr = thr;
		}

		@Override
		public void renderInPage(Output out) throws IOException {
			out.startPage(this);
			
			Throwable t = thr;
			
			while(t != null) {
				if(t != thr) {
					out.startPage(null);
					out.startPar();
					out.startBold();
					out.outputText("Caused by:");
					out.endBold();
					out.endPar();
				}
				
				out.startTable();
				Lathos.row(out, "Class", t.getClass().getName());
				Lathos.row(out, "Message", t.getMessage());
				Lathos.row(out, "Object", t);
				out.endTable();
				
				out.startPage(null);
				out.startTable();
				for(StackTraceElement elem : t.getStackTrace()) {
					Lathos.row(out, elem.toString());
				}
				out.endTable();
				out.endPage(null);
				
				if(t != thr) {
					out.endPage(null);
				}

				t = t.getCause();
			}
			
			out.endPage(this);
		}

		@Override
		public void renderInLine(Output output) throws IOException {
			Lathos.renderInLine(this, output);
		}
		
		@Override
		public String toString() {
			return getId();
		}

		@Override
		public String getId() {
			return String.format("Throwable[%x]", System.identityHashCode(thr));
		}

		@Override
		public Page getParent() {
			return null;
		}

		@Override
		public void addContent(PageContent content) {
			throw new UnsupportedOperationException();
		}
		
	}

	@Override
	public boolean addToLine(Line line, Object object) {
		if(object instanceof Throwable) {
			line.addContent(new ThrowablePage((Throwable)object));
			return true;
		}
		return false;
	}

}
