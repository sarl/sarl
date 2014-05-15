/**
 * 
 */
package io.sarl.eclipse.navigator;

import org.eclipse.core.resources.IProject;
import org.eclipse.swt.graphics.Image;

/**
 * A SARL Project Element
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface ISARLProjectElement {
	
    public Image getImage();
    
    public Object[] getChildren();
 
    public String getText();
 
    public boolean hasChildren();
 
    public IProject getProject();
 
    public Object getParent();
}
