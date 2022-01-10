/**
 */
package io.sarl.lang.sarl.util;

import io.sarl.lang.sarl.*;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.xtend.core.xtend.XtendAnnotationTarget;
import org.eclipse.xtend.core.xtend.XtendAnnotationType;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendEnum;
import org.eclipse.xtend.core.xtend.XtendEnumLiteral;
import org.eclipse.xtend.core.xtend.XtendExecutable;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;

import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XExpression;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see io.sarl.lang.sarl.SarlPackage
 * @generated
 */
public class SarlAdapterFactory extends AdapterFactoryImpl
{
	/**
	 * The cached model package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static SarlPackage modelPackage;

	/**
	 * Creates an instance of the adapter factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SarlAdapterFactory()
	{
		if (modelPackage == null)
		{
			modelPackage = SarlPackage.eINSTANCE;
		}
	}

	/**
	 * Returns whether this factory is applicable for the type of the object.
	 * <!-- begin-user-doc -->
	 * This implementation returns <code>true</code> if the object is either the model's package or is an instance object of the model.
	 * <!-- end-user-doc -->
	 * @return whether this factory is applicable for the type of the object.
	 * @generated
	 */
	@Override
	public boolean isFactoryForType(Object object)
	{
		if (object == modelPackage)
		{
			return true;
		}
		if (object instanceof EObject)
		{
			return ((EObject)object).eClass().getEPackage() == modelPackage;
		}
		return false;
	}

	/**
	 * The switch that delegates to the <code>createXXX</code> methods.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SarlSwitch<Adapter> modelSwitch =
		new SarlSwitch<Adapter>()
		{
			@Override
			public Adapter caseSarlScript(SarlScript object)
			{
				return createSarlScriptAdapter();
			}
			@Override
			public Adapter caseSarlField(SarlField object)
			{
				return createSarlFieldAdapter();
			}
			@Override
			public Adapter caseSarlBreakExpression(SarlBreakExpression object)
			{
				return createSarlBreakExpressionAdapter();
			}
			@Override
			public Adapter caseSarlContinueExpression(SarlContinueExpression object)
			{
				return createSarlContinueExpressionAdapter();
			}
			@Override
			public Adapter caseSarlAssertExpression(SarlAssertExpression object)
			{
				return createSarlAssertExpressionAdapter();
			}
			@Override
			public Adapter caseSarlAction(SarlAction object)
			{
				return createSarlActionAdapter();
			}
			@Override
			public Adapter caseSarlConstructor(SarlConstructor object)
			{
				return createSarlConstructorAdapter();
			}
			@Override
			public Adapter caseSarlBehaviorUnit(SarlBehaviorUnit object)
			{
				return createSarlBehaviorUnitAdapter();
			}
			@Override
			public Adapter caseSarlCapacityUses(SarlCapacityUses object)
			{
				return createSarlCapacityUsesAdapter();
			}
			@Override
			public Adapter caseSarlRequiredCapacity(SarlRequiredCapacity object)
			{
				return createSarlRequiredCapacityAdapter();
			}
			@Override
			public Adapter caseSarlClass(SarlClass object)
			{
				return createSarlClassAdapter();
			}
			@Override
			public Adapter caseSarlInterface(SarlInterface object)
			{
				return createSarlInterfaceAdapter();
			}
			@Override
			public Adapter caseSarlEnumeration(SarlEnumeration object)
			{
				return createSarlEnumerationAdapter();
			}
			@Override
			public Adapter caseSarlAnnotationType(SarlAnnotationType object)
			{
				return createSarlAnnotationTypeAdapter();
			}
			@Override
			public Adapter caseSarlEnumLiteral(SarlEnumLiteral object)
			{
				return createSarlEnumLiteralAdapter();
			}
			@Override
			public Adapter caseSarlEvent(SarlEvent object)
			{
				return createSarlEventAdapter();
			}
			@Override
			public Adapter caseSarlCastedExpression(SarlCastedExpression object)
			{
				return createSarlCastedExpressionAdapter();
			}
			@Override
			public Adapter caseSarlSpace(SarlSpace object)
			{
				return createSarlSpaceAdapter();
			}
			@Override
			public Adapter caseSarlArtifact(SarlArtifact object)
			{
				return createSarlArtifactAdapter();
			}
			@Override
			public Adapter caseSarlAgent(SarlAgent object)
			{
				return createSarlAgentAdapter();
			}
			@Override
			public Adapter caseSarlCapacity(SarlCapacity object)
			{
				return createSarlCapacityAdapter();
			}
			@Override
			public Adapter caseSarlBehavior(SarlBehavior object)
			{
				return createSarlBehaviorAdapter();
			}
			@Override
			public Adapter caseSarlSkill(SarlSkill object)
			{
				return createSarlSkillAdapter();
			}
			@Override
			public Adapter caseSarlFormalParameter(SarlFormalParameter object)
			{
				return createSarlFormalParameterAdapter();
			}
			@Override
			public Adapter caseXtendFile(XtendFile object)
			{
				return createXtendFileAdapter();
			}
			@Override
			public Adapter caseXtendAnnotationTarget(XtendAnnotationTarget object)
			{
				return createXtendAnnotationTargetAdapter();
			}
			@Override
			public Adapter caseXtendMember(XtendMember object)
			{
				return createXtendMemberAdapter();
			}
			@Override
			public Adapter caseXtendField(XtendField object)
			{
				return createXtendFieldAdapter();
			}
			@Override
			public Adapter caseXExpression(XExpression object)
			{
				return createXExpressionAdapter();
			}
			@Override
			public Adapter caseXtendExecutable(XtendExecutable object)
			{
				return createXtendExecutableAdapter();
			}
			@Override
			public Adapter caseXtendFunction(XtendFunction object)
			{
				return createXtendFunctionAdapter();
			}
			@Override
			public Adapter caseXtendConstructor(XtendConstructor object)
			{
				return createXtendConstructorAdapter();
			}
			@Override
			public Adapter caseXtendTypeDeclaration(XtendTypeDeclaration object)
			{
				return createXtendTypeDeclarationAdapter();
			}
			@Override
			public Adapter caseXtendClass(XtendClass object)
			{
				return createXtendClassAdapter();
			}
			@Override
			public Adapter caseXtendInterface(XtendInterface object)
			{
				return createXtendInterfaceAdapter();
			}
			@Override
			public Adapter caseXtendEnum(XtendEnum object)
			{
				return createXtendEnumAdapter();
			}
			@Override
			public Adapter caseXtendAnnotationType(XtendAnnotationType object)
			{
				return createXtendAnnotationTypeAdapter();
			}
			@Override
			public Adapter caseXtendEnumLiteral(XtendEnumLiteral object)
			{
				return createXtendEnumLiteralAdapter();
			}
			@Override
			public Adapter caseXCastedExpression(XCastedExpression object)
			{
				return createXCastedExpressionAdapter();
			}
			@Override
			public Adapter caseXtendParameter(XtendParameter object)
			{
				return createXtendParameterAdapter();
			}
			@Override
			public Adapter defaultCase(EObject object)
			{
				return createEObjectAdapter();
			}
		};

	/**
	 * Creates an adapter for the <code>target</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param target the object to adapt.
	 * @return the adapter for the <code>target</code>.
	 * @generated
	 */
	@Override
	public Adapter createAdapter(Notifier target)
	{
		return modelSwitch.doSwitch((EObject)target);
	}


	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlScript <em>Script</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlScript
	 * @generated
	 */
	public Adapter createSarlScriptAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlField <em>Field</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlField
	 * @generated
	 */
	public Adapter createSarlFieldAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlBreakExpression <em>Break Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlBreakExpression
	 * @since 0.5
	 * @generated
	 */
	public Adapter createSarlBreakExpressionAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlContinueExpression <em>Continue Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlContinueExpression
	 * @since 0.7
	 * @generated
	 */
	public Adapter createSarlContinueExpressionAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlAssertExpression <em>Assert Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlAssertExpression
	 * @since 0.6
	 * @generated
	 */
	public Adapter createSarlAssertExpressionAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlAction <em>Action</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlAction
	 * @generated
	 */
	public Adapter createSarlActionAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlConstructor <em>Constructor</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlConstructor
	 * @generated
	 */
	public Adapter createSarlConstructorAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlBehaviorUnit <em>Behavior Unit</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlBehaviorUnit
	 * @generated
	 */
	public Adapter createSarlBehaviorUnitAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlCapacityUses <em>Capacity Uses</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlCapacityUses
	 * @generated
	 */
	public Adapter createSarlCapacityUsesAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlRequiredCapacity <em>Required Capacity</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlRequiredCapacity
	 * @generated
	 */
	public Adapter createSarlRequiredCapacityAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlClass <em>Class</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlClass
	 * @generated
	 */
	public Adapter createSarlClassAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlInterface <em>Interface</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlInterface
	 * @generated
	 */
	public Adapter createSarlInterfaceAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlEnumeration <em>Enumeration</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlEnumeration
	 * @generated
	 */
	public Adapter createSarlEnumerationAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlAnnotationType <em>Annotation Type</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlAnnotationType
	 * @generated
	 */
	public Adapter createSarlAnnotationTypeAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlEnumLiteral <em>Enum Literal</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlEnumLiteral
	 * @generated
	 */
	public Adapter createSarlEnumLiteralAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlEvent <em>Event</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlEvent
	 * @generated
	 */
	public Adapter createSarlEventAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlCastedExpression <em>Casted Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlCastedExpression
	 * @since 0.9
	 * @generated
	 */
	public Adapter createSarlCastedExpressionAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlSpace <em>Space</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlSpace
	 * @generated
	 */
	public Adapter createSarlSpaceAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlArtifact <em>Artifact</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlArtifact
	 * @generated
	 */
	public Adapter createSarlArtifactAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlAgent <em>Agent</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlAgent
	 * @generated
	 */
	public Adapter createSarlAgentAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlCapacity <em>Capacity</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlCapacity
	 * @generated
	 */
	public Adapter createSarlCapacityAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlBehavior <em>Behavior</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlBehavior
	 * @generated
	 */
	public Adapter createSarlBehaviorAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlSkill <em>Skill</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlSkill
	 * @generated
	 */
	public Adapter createSarlSkillAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlFormalParameter <em>Formal Parameter</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see io.sarl.lang.sarl.SarlFormalParameter
	 * @generated
	 */
	public Adapter createSarlFormalParameterAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.xtend.core.xtend.XtendFile <em>File</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.xtend.core.xtend.XtendFile
	 * @generated
	 */
	public Adapter createXtendFileAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.xtend.core.xtend.XtendAnnotationTarget <em>Annotation Target</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.xtend.core.xtend.XtendAnnotationTarget
	 * @generated
	 */
	public Adapter createXtendAnnotationTargetAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.xtend.core.xtend.XtendMember <em>Member</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.xtend.core.xtend.XtendMember
	 * @generated
	 */
	public Adapter createXtendMemberAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.xtend.core.xtend.XtendField <em>Field</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.xtend.core.xtend.XtendField
	 * @generated
	 */
	public Adapter createXtendFieldAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.xtext.xbase.XExpression <em>XExpression</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.xtext.xbase.XExpression
	 * @since 2.7
	 * @generated
	 */
	public Adapter createXExpressionAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.xtend.core.xtend.XtendExecutable <em>Executable</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.xtend.core.xtend.XtendExecutable
	 * @generated
	 */
	public Adapter createXtendExecutableAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.xtend.core.xtend.XtendFunction <em>Function</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.xtend.core.xtend.XtendFunction
	 * @generated
	 */
	public Adapter createXtendFunctionAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.xtend.core.xtend.XtendConstructor <em>Constructor</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.xtend.core.xtend.XtendConstructor
	 * @generated
	 */
	public Adapter createXtendConstructorAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.xtend.core.xtend.XtendTypeDeclaration <em>Type Declaration</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.xtend.core.xtend.XtendTypeDeclaration
	 * @generated
	 */
	public Adapter createXtendTypeDeclarationAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.xtend.core.xtend.XtendClass <em>Class</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.xtend.core.xtend.XtendClass
	 * @generated
	 */
	public Adapter createXtendClassAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.xtend.core.xtend.XtendInterface <em>Interface</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.xtend.core.xtend.XtendInterface
	 * @generated
	 */
	public Adapter createXtendInterfaceAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.xtend.core.xtend.XtendEnum <em>Enum</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.xtend.core.xtend.XtendEnum
	 * @generated
	 */
	public Adapter createXtendEnumAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.xtend.core.xtend.XtendAnnotationType <em>Annotation Type</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.xtend.core.xtend.XtendAnnotationType
	 * @generated
	 */
	public Adapter createXtendAnnotationTypeAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.xtend.core.xtend.XtendEnumLiteral <em>Enum Literal</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.xtend.core.xtend.XtendEnumLiteral
	 * @generated
	 */
	public Adapter createXtendEnumLiteralAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.xtext.xbase.XCastedExpression <em>XCasted Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.xtext.xbase.XCastedExpression
	 * @since 2.7
	 * @generated
	 */
	public Adapter createXCastedExpressionAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.xtend.core.xtend.XtendParameter <em>Parameter</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.xtend.core.xtend.XtendParameter
	 * @generated
	 */
	public Adapter createXtendParameterAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for the default case.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @generated
	 */
	public Adapter createEObjectAdapter()
	{
		return null;
	}

} //SarlAdapterFactory
