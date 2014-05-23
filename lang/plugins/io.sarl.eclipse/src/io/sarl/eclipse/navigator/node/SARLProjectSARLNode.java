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
		this.parent = iparent;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see io.sarl.eclipse.navigator.ISARLProjectElement#getImage()
	 */
	public Image getImage() {
		if (this.image == null) {
			this.image = EclipseSARLActivator.getImage("icons/sarl-project-schema-16.png"); //$NON-NLS-1$
		}
		return this.image;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see io.sarl.eclipse.navigator.ISARLProjectElement#getChildren()
	 */
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
	public String getText() {
		return NAME;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see io.sarl.eclipse.navigator.ISARLProjectElement#hasChildren()
	 */
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
	public IProject getProject() {
		return this.parent.getProject();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see io.sarl.eclipse.navigator.ISARLProjectElement#getParent()
	 */
	public Object getParent() {
		return this.parent;
	}

}
