package sootproxy;

import java.util.Map;

import soot.SceneTransformer;

import sootproxy.ScalaSceneTransformer;

public class SceneTransformerProxy extends SceneTransformer {
	private ScalaSceneTransformer sct;

	public SceneTransformerProxy(ScalaSceneTransformer sct)
	{ this.sct = sct; }
	
	@Override
	protected void internalTransform(
			String phaseName,
			@SuppressWarnings("rawtypes") Map options
	) { sct.internalTransform(phaseName); }
	
}
