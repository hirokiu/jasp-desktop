import QtQuick 2.9
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import JASP.Theme 1.0

Rectangle
{
	id:			rect
	objectName: "rect"
	color:		Theme.grayMuchLighter

	Label
	{
		id:headLabel
		width:400
		height:30
		anchors.top: parent.top
		anchors.left: parent.left  //Position Recent Files label
		anchors.leftMargin: 12
		anchors.topMargin: 12
		text: "Recent Files"
		font.family: "SansSerif"
		font.pixelSize: 18
		color: Theme.black
	}

	ToolSeparator
	{
		id: firstSeparator
		anchors.top: headLabel.bottom
		width: rect.width
		orientation: Qt.Horizontal
	}

	FileList {
		id:			recentFilesList
		cppModel:	fileMenuModel.recentFiles.listModel

		anchors
		{
			top:			firstSeparator.bottom
			left:			parent.left
			right:			parent.right
			bottom:			parent.bottom
			leftMargin:		12  //Position datalibrary items
			topMargin:		6
			bottomMargin:	6
		}
	}
}
