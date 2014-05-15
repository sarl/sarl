/**
 * 
 */
package io.sarl.eclipse.navigator.node;

import io.sarl.eclipse.internal.EclipseSARLActivator;
import io.sarl.eclipse.navigator.ISARLProjectElement;

import org.eclipse.core.resources.IProject;
import org.eclipse.swt.graphics.Image;

/**
 * The node of SARL Project called SARL just below the root node
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLProjectSARLNode implements ISARLProjectElement {


	public static final String NAME = "SARL"; //$NON-NLS-1$
	
	private ISARLProjectElement parent;
	private ISARLProjectElement[] children;
	private Image image;

	public SARLProjectSARLNode(ISARLProjectElement iparent) {
		parent = iparent;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see io.sarl.eclipse.navigator.ISARLProjectElement#getImage()
	 */
	@Override
	public Image getImage() {
		if (image == null) {
			image = EclipseSARLActivator.getImage("icons/sarl-project-schema-16.png"); //$NON-NLS-1$
		}
		return image;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see io.sarl.eclipse.navigator.ISARLProjectElement#getChildren()
	 */
	@Override
	public Object[] getChildren() {
		if (this.children == null) {
			this.children = initializeChildren(getProject());
		}
		// else the children are just fine

		return this.children;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see io.sarl.eclipse.navigator.ISARLProjectElement#getText()
	 */
	@Override
	public String getText() {
		return NAME;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see io.sarl.eclipse.navigator.ISARLProjectElement#hasChildren()
	 */
	@Override
	public boolean hasChildren() {
		if (this.children == null) {
			this.children = initializeChildren(getProject());
		}
		// else we have already initialized them

		return this.children.length > 0;
	}
	
    private ISARLProjectElement[] initializeChildren(IProject iProject) {
		ISARLProjectElement[] ichildren = new ISARLProjectElement[0];

		return ichildren;
    }

	/*
	 * (non-Javadoc)
	 * 
	 * @see io.sarl.eclipse.navigator.ISARLProjectElement#getProject()
	 */
	@Override
	public IProject getProject() {
		return parent.getProject();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see io.sarl.eclipse.navigator.ISARLProjectElement#getParent()
	 */
	@Override
	public Object getParent() {
		return parent;
	}

}
