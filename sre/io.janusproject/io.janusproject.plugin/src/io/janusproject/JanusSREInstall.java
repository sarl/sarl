/**
 * 
 */
package io.janusproject;

import java.io.IOException;
import java.util.Map;

import javax.naming.NameNotFoundException;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.launching.LibraryLocation;
import org.osgi.framework.Bundle;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import io.sarl.eclipse.runtime.AbstractSREInstall;
import io.sarl.eclipse.runtime.StandardSREInstall;
import io.sarl.eclipse.util.BundleUtil;


/**
 * Provide Janus as a SRE install.
 *
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JanusSREInstall extends StandardSREInstall {
	
	
	public JanusSREInstall(String id) {
		super(id);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getNameNoDefault() {
		return "JANUS integrated SRE plugin";
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getLocation() {
		final Bundle bundle = Platform.getBundle(this.getClass().getCanonicalName());
		

		final IPath bundlePath = BundleUtil.getBundlePath(bundle);
		final IPath sourceBundlePath = BundleUtil.getSourceBundlePath(bundle, bundlePath);
		return null;
	}





}
