/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.FeatureContainer;
import io.sarl.lang.sarl.ImplementingElement;
import io.sarl.lang.sarl.InheritingElement;
import io.sarl.lang.sarl.NamedElement;
import io.sarl.lang.sarl.SarlPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Implementing Element</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.impl.ImplementingElementImpl#getName <em>Name</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.ImplementingElementImpl#getFeatures <em>Features</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.ImplementingElementImpl#getSuperTypes <em>Super Types</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.ImplementingElementImpl#getImplementedTypes <em>Implemented Types</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ImplementingElementImpl extends TopElementImpl implements ImplementingElement
{
  /**
   * The default value of the '{@link #getName() <em>Name</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getName()
   * @generated
   * @ordered
   */
  protected static final String NAME_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getName() <em>Name</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getName()
   * @generated
   * @ordered
   */
  protected String name = NAME_EDEFAULT;

  /**
   * The cached value of the '{@link #getFeatures() <em>Features</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getFeatures()
   * @generated
   * @ordered
   */
  protected EList<EObject> features;

  /**
   * The cached value of the '{@link #getSuperTypes() <em>Super Types</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getSuperTypes()
   * @generated
   * @ordered
   */
  protected EList<JvmParameterizedTypeReference> superTypes;

  /**
   * The cached value of the '{@link #getImplementedTypes() <em>Implemented Types</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getImplementedTypes()
   * @generated
   * @ordered
   */
  protected EList<JvmParameterizedTypeReference> implementedTypes;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected ImplementingElementImpl()
  {
    super();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  protected EClass eStaticClass()
  {
    return SarlPackage.Literals.IMPLEMENTING_ELEMENT;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getName()
  {
    return name;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setName(String newName)
  {
    String oldName = name;
    name = newName;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.IMPLEMENTING_ELEMENT__NAME, oldName, name));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<EObject> getFeatures()
  {
    if (features == null)
    {
      features = new EObjectContainmentEList<EObject>(EObject.class, this, SarlPackage.IMPLEMENTING_ELEMENT__FEATURES);
    }
    return features;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<JvmParameterizedTypeReference> getSuperTypes()
  {
    if (superTypes == null)
    {
      superTypes = new EObjectContainmentEList<JvmParameterizedTypeReference>(JvmParameterizedTypeReference.class, this, SarlPackage.IMPLEMENTING_ELEMENT__SUPER_TYPES);
    }
    return superTypes;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<JvmParameterizedTypeReference> getImplementedTypes()
  {
    if (implementedTypes == null)
    {
      implementedTypes = new EObjectContainmentEList<JvmParameterizedTypeReference>(JvmParameterizedTypeReference.class, this, SarlPackage.IMPLEMENTING_ELEMENT__IMPLEMENTED_TYPES);
    }
    return implementedTypes;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs)
  {
    switch (featureID)
    {
      case SarlPackage.IMPLEMENTING_ELEMENT__FEATURES:
        return ((InternalEList<?>)getFeatures()).basicRemove(otherEnd, msgs);
      case SarlPackage.IMPLEMENTING_ELEMENT__SUPER_TYPES:
        return ((InternalEList<?>)getSuperTypes()).basicRemove(otherEnd, msgs);
      case SarlPackage.IMPLEMENTING_ELEMENT__IMPLEMENTED_TYPES:
        return ((InternalEList<?>)getImplementedTypes()).basicRemove(otherEnd, msgs);
    }
    return super.eInverseRemove(otherEnd, featureID, msgs);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public Object eGet(int featureID, boolean resolve, boolean coreType)
  {
    switch (featureID)
    {
      case SarlPackage.IMPLEMENTING_ELEMENT__NAME:
        return getName();
      case SarlPackage.IMPLEMENTING_ELEMENT__FEATURES:
        return getFeatures();
      case SarlPackage.IMPLEMENTING_ELEMENT__SUPER_TYPES:
        return getSuperTypes();
      case SarlPackage.IMPLEMENTING_ELEMENT__IMPLEMENTED_TYPES:
        return getImplementedTypes();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @SuppressWarnings("unchecked")
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case SarlPackage.IMPLEMENTING_ELEMENT__NAME:
        setName((String)newValue);
        return;
      case SarlPackage.IMPLEMENTING_ELEMENT__FEATURES:
        getFeatures().clear();
        getFeatures().addAll((Collection<? extends EObject>)newValue);
        return;
      case SarlPackage.IMPLEMENTING_ELEMENT__SUPER_TYPES:
        getSuperTypes().clear();
        getSuperTypes().addAll((Collection<? extends JvmParameterizedTypeReference>)newValue);
        return;
      case SarlPackage.IMPLEMENTING_ELEMENT__IMPLEMENTED_TYPES:
        getImplementedTypes().clear();
        getImplementedTypes().addAll((Collection<? extends JvmParameterizedTypeReference>)newValue);
        return;
    }
    super.eSet(featureID, newValue);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eUnset(int featureID)
  {
    switch (featureID)
    {
      case SarlPackage.IMPLEMENTING_ELEMENT__NAME:
        setName(NAME_EDEFAULT);
        return;
      case SarlPackage.IMPLEMENTING_ELEMENT__FEATURES:
        getFeatures().clear();
        return;
      case SarlPackage.IMPLEMENTING_ELEMENT__SUPER_TYPES:
        getSuperTypes().clear();
        return;
      case SarlPackage.IMPLEMENTING_ELEMENT__IMPLEMENTED_TYPES:
        getImplementedTypes().clear();
        return;
    }
    super.eUnset(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public boolean eIsSet(int featureID)
  {
    switch (featureID)
    {
      case SarlPackage.IMPLEMENTING_ELEMENT__NAME:
        return NAME_EDEFAULT == null ? name != null : !NAME_EDEFAULT.equals(name);
      case SarlPackage.IMPLEMENTING_ELEMENT__FEATURES:
        return features != null && !features.isEmpty();
      case SarlPackage.IMPLEMENTING_ELEMENT__SUPER_TYPES:
        return superTypes != null && !superTypes.isEmpty();
      case SarlPackage.IMPLEMENTING_ELEMENT__IMPLEMENTED_TYPES:
        return implementedTypes != null && !implementedTypes.isEmpty();
    }
    return super.eIsSet(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public int eBaseStructuralFeatureID(int derivedFeatureID, Class<?> baseClass)
  {
    if (baseClass == NamedElement.class)
    {
      switch (derivedFeatureID)
      {
        case SarlPackage.IMPLEMENTING_ELEMENT__NAME: return SarlPackage.NAMED_ELEMENT__NAME;
        default: return -1;
      }
    }
    if (baseClass == FeatureContainer.class)
    {
      switch (derivedFeatureID)
      {
        case SarlPackage.IMPLEMENTING_ELEMENT__FEATURES: return SarlPackage.FEATURE_CONTAINER__FEATURES;
        default: return -1;
      }
    }
    if (baseClass == InheritingElement.class)
    {
      switch (derivedFeatureID)
      {
        case SarlPackage.IMPLEMENTING_ELEMENT__SUPER_TYPES: return SarlPackage.INHERITING_ELEMENT__SUPER_TYPES;
        default: return -1;
      }
    }
    return super.eBaseStructuralFeatureID(derivedFeatureID, baseClass);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public int eDerivedStructuralFeatureID(int baseFeatureID, Class<?> baseClass)
  {
    if (baseClass == NamedElement.class)
    {
      switch (baseFeatureID)
      {
        case SarlPackage.NAMED_ELEMENT__NAME: return SarlPackage.IMPLEMENTING_ELEMENT__NAME;
        default: return -1;
      }
    }
    if (baseClass == FeatureContainer.class)
    {
      switch (baseFeatureID)
      {
        case SarlPackage.FEATURE_CONTAINER__FEATURES: return SarlPackage.IMPLEMENTING_ELEMENT__FEATURES;
        default: return -1;
      }
    }
    if (baseClass == InheritingElement.class)
    {
      switch (baseFeatureID)
      {
        case SarlPackage.INHERITING_ELEMENT__SUPER_TYPES: return SarlPackage.IMPLEMENTING_ELEMENT__SUPER_TYPES;
        default: return -1;
      }
    }
    return super.eDerivedStructuralFeatureID(baseFeatureID, baseClass);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public String toString()
  {
    if (eIsProxy()) return super.toString();

    StringBuffer result = new StringBuffer(super.toString());
    result.append(" (name: ");
    result.append(name);
    result.append(')');
    return result.toString();
  }

} //ImplementingElementImpl
