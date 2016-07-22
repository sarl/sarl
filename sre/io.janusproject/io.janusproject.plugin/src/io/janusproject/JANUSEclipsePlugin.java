/**
 * 
 */
package io.janusproject;

import org.eclipse.ui.plugin.AbstractUIPlugin;

import io.sarl.eclipse.SARLEclipsePlugin;

/**
 * @author ngaud
 *
 */
public class JANUSEclipsePlugin extends AbstractUIPlugin {

	/** Identifier of the plugin.
	 */
	public static final String PLUGIN_ID = "io.janusproject.plugin"; //$NON-NLS-1$
	
	private static JANUSEclipsePlugin instance;

	/** Construct an Eclipse plugin for SARL.
	 */
	public JANUSEclipsePlugin() {
		setDefault(this);
	}
	
	
	/** Set the default instance of the plugin.
	 *
	 * @param defaultInstance - the default plugin instance.
	 */
	public static void setDefault(JANUSEclipsePlugin defaultInstance) {
		instance = defaultInstance;
	}

	/** Replies the instance of the plugin.
	 *
	 * @return the default plugin instance.
	 */
	public static JANUSEclipsePlugin getDefault() {
		return instance;
	}
}
